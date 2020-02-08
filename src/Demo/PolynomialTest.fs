module PolynomialTest

open System
open Aardvark.Base

let run() = 
    let scale = 1.0
    let cnt = 15000
    let minDegree = 3
    let maxDegree = 30


    let getPoly (zeros : seq<float>) =
        let mutable z = [|1.0|]
        for zero in zeros do
            z <- Polynomial.Multiply(z, [| zero; -1.0 |])
        z


    let rand = System.Random()



    let manyRoots = Array.init (cnt * maxDegree) (fun _ -> (rand.NextDouble() - 0.5) * scale)

    for degree in minDegree .. maxDegree do
        let mutable histo = MapExt.empty
        Log.start "degree %d" degree
        let scale2 = scale * scale

        let polys =
            Array.init cnt (fun k ->
                let bi = k * degree
                let roots = List.init degree (fun i -> manyRoots.[bi + i]) |> List.sort
                roots, getPoly roots
            )


        let results = 
            System.GC.Collect()
            System.GC.WaitForFullGCComplete() |> ignore
            Array.init cnt id |> Array.Parallel.choose (fun k ->
            //for k in 0 .. cnt - 1 do
                //let bi = k * degree
                //let roots = List.init degree (fun i -> manyRoots.[bi + i]) |> List.sort
                let roots, poly = polys.[k]

                let iter, r = Polynomial.roots 1E-3 poly
                if iter = Int32.MaxValue then
                    None
                else
                    let errors = 
                        (roots, r) 
                        ||> List.map2 (fun real sol -> (real - sol).NormSquared / scale2)
            
                    let rmse = List.average errors
                    let max = List.max errors
                    let min = List.min errors
                
                    Some (struct (iter, rmse, min, max))
                )


        let failed = cnt - results.Length
        //let cnt = results.Length
        let mutable sumIter = 0.0
        for struct (iter, rmse, min, max) in results do
            sumIter <- sumIter + float iter
            let l = log rmse / log 10.0
            if l >= -30.0 then
                let k = int (round l)
                histo <-
                    histo |> MapExt.alter k (fun o ->
                        1 + (defaultArg o 0) |> Some
                    )
            else
                let k = Int32.MinValue
                histo <-
                    histo |> MapExt.alter k (fun o ->
                        1 + (defaultArg o 0) |> Some
                    )
                
        let avgIter = sumIter / float cnt

        let zero, histo =
            match MapExt.tryRemove Int32.MinValue histo with
            | Some (cnt, rest) -> cnt, rest
            | None -> 0, histo
        let minE = -30 //MapExt.min histo
        let maxE = MapExt.tryMax histo |> Option.defaultValue -31

        let lines =
            [ minE .. maxE ] |> List.map (fun e ->
                let c = MapExt.tryFind e histo |> Option.defaultValue 0
                sprintf "1E%d: " e, float c / float cnt
            )

        let lines =
            if zero > 0 then 
                let l0 = "zero: ", float zero / float cnt
                l0 :: lines
            else
                lines

        let lines =
            if failed > 0 then
                ("failed: ", float failed / float cnt) :: lines
            else    
                lines

        let longest = lines |> List.map (fun (k,_) -> k.Length) |> List.max
        let pad (len : int) (str : string) =
            if str.Length < len then str + System.String(' ', len - str.Length)
            else str

        let barWidth = 20

        let maxValue = lines |> List.map snd |> List.max

        for(h,v) in lines do
            let h = pad longest h

            let w = round (float barWidth * (v / maxValue)) |> int
            let bar = System.String('#', w) |> pad barWidth
            if h.StartsWith "failed" then Report.WarnNoPrefix(sprintf "%s%s (%.2f%%)" h bar (100.0 * v))
            else Log.line "%s%s (%.2f%%)" h bar (100.0 * v)

        Log.line "avg %.2f iter" avgIter

        Log.stop()