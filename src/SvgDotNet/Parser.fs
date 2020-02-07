namespace SvgDotNet

open Aardvark.Base

module Parser = 
    open System.Text.RegularExpressions
    open System.Globalization

    let private floatRx = "[ \t]*([-+]?[0-9]*\.?[0-9]+(?:[eE][-+]?[0-9]+)?)"

    [<Struct; StructuredFormatDisplay("{AsString}")>]
    type Text private(_u : unit, data : string, start : int, length : int) =
        override x.ToString() = data.Substring(start, length)
        member private x.AsString = x.ToString()

        member x.Data = data
        member x.Start = start
        member x.Length = length

        member x.Substring(offset : int, len : int) =
            if offset < 0 || offset + len > length then raise <| System.IndexOutOfRangeException()
            Text(data, start + offset, len)
            
        member x.Substring(offset : int) =
            if offset < 0 || offset > length then raise <| System.IndexOutOfRangeException()
            Text(data, start + offset, length - offset)
    
        
        static member IsEmpty(t : Text) =
            t.Length = 0

        static member IsEmptyOrWhitespace(t : Text) =
            t.Length = 0 || t.Forall(fun c -> c = ' ' || c = '\t' || c = '\r' || c = '\n')

        member x.Forall(predicate : char -> bool) =
            let mutable i = start
            let e = start + length - 1
            let mutable t = true
            while t && i < e do
                if not (predicate data.[i]) then t <- false
                i <- i + 1
            t
        
        member x.Item
            with get(i : int) =
                if i < 0 || i >= length then raise <| System.IndexOutOfRangeException()
                data.[start + i]

        member x.TrimStart() =
            let mutable o = 0
            let mutable s = start
            while o < length && (data.[s] = ' ' || data.[s] = '\t') do
                s <- s + 1
                o <- o + 1
            Text(data, s, length - o)

        member x.StartsWith (str : string) =
            if str.Length <= length then
                let mutable i = start
                let mutable j = 0
                let mutable valid = true
                while valid && j < str.Length do    
                    if data.[i] <> str.[j] then
                        valid <- false
                    i <- i + 1
                    j <- j + 1
                valid
            else
                false


        member x.StartsWith (str : Text) =
            if str.Length <= length then
                let mutable i = start
                let mutable j = str.Start
                let e = str.Start + str.Length
                let mutable valid = true
                while valid && j < e do    
                    if data.[i] <> str.Data.[j] then
                        valid <- false
                    i <- i + 1
                    j <- j + 1
                valid
            else
                false



        new(str : string) = 
            Text((), str, 0, str.Length)

        new(str : string, offset : int) = 
            if offset < 0 || offset > str.Length then raise <| System.IndexOutOfRangeException()
            Text((), str, offset, str.Length - offset)

        new(str : string, offset : int, length : int) = 
            if offset < 0 || offset + length > str.Length then raise <| System.IndexOutOfRangeException()
            Text((), str, offset, length)

    type System.Text.RegularExpressions.Regex with
        member x.Match(t : Text) =
            x.Match(t.Data, t.Start, t.Length)

    type Parser<'a> =
        Text -> Option<Text * 'a>

    let (.>.) (l : Parser<'a>) (r : Parser<'b>) =
        fun str ->
            match l str with
            | Some (str, a) ->
                match r str with
                | Some (str, b) ->
                    Some (str, (a,b))
                | None ->
                    None
            | None -> 
                None

    let (>.) (l : Parser<'a>) (r : Parser<'b>) =
        fun str ->
            match l str with
            | Some (str, a) ->
                match r str with
                | Some (str, b) ->
                    Some (str, b)
                | None ->
                    None
            | None -> 
                None

    let (.>) (l : Parser<'a>) (r : Parser<'b>) =
        fun str ->
            match l str with
            | Some (str, a) ->
                match r str with
                | Some (str, b) ->
                    Some (str, a)
                | None ->
                    None
            | None -> 
                None

    let pChoice (parsers : list<Parser<'a>>) =
        fun str ->
            parsers |> List.tryPick (fun p -> p str)

    let pMap (mapping : 'a -> 'b) (p : Parser<'a>) =
        fun str ->
            match p str with
            | Some (str, a) -> Some (str, mapping a)
            | None -> None
            
    let pBind (mapping : 'a -> Parser<'b>) (p : Parser<'a>) : Parser<'b> =
        fun str ->
            match p str with
            | Some (str, a) -> mapping a str
            | None -> None

    let pRegex parts : Parser<Match> =
        let pat = String.concat "" parts
        let pat = if not (pat.StartsWith "^") then "^" + pat else pat
        let rx = Regex pat
        fun str ->
            let m = rx.Match str
            if m.Success then Some(str.Substring(m.Index - str.Start + m.Length), m)
            else None
  
    let pSuccess (value : 'a) : Parser<'a> =
        fun str ->
            Some(str, value)

    let rec pMany (parser : Parser<'a>) =
        fun str0 ->
            if Text.IsEmptyOrWhitespace str0 then
                Some(str0, [])
            else
                match parser str0 with
                | Some(str1, h) ->  
                    match pMany parser str1 with
                    | Some(str, t) -> Some (str, h :: t)
                    | None -> Some(str1, [h])
                | None ->
                    Some(str0, [])

    let pMany1 (parser : Parser<'a>) =
        parser .>. pMany parser |> pMap (fun (h,t) -> h :: t)
        
    let pFloat : Parser<float> =
        pRegex [floatRx] |> pMap (fun m -> float m.Groups.[1].Value)

    let pInt =
        pRegex [ "[ \t]*((?:0x[0-9A-Fa-f]+)|(?:[0-9]+))"] |> 
        pMap (fun m ->
            let v = m.Groups.[1].Value
            if v.StartsWith "0x" then System.Int32.Parse(v.Substring 2, NumberStyles.HexNumber)
            else System.Int32.Parse(v)
        )

    let pWhitespace =
        pRegex [ "[ \t]*" ] |> pMap ignore

    let pAttempt (p : Parser<'a>) =
        fun str ->
            match p str with
            | Some (str, r) -> Some (str, Some r)
            | None -> Some (str, None)
 
    let pAnyString (strs : list<string * 'a>) =
        fun (input : Text) ->
            let input = input.TrimStart()
            strs |> List.tryPick (fun (str,value) ->
                if input.StartsWith str then Some(input.Substring str.Length, value)
                else None

            )
        
    let pSepBy1 (sep : Parser<'x>) (parser : Parser<'a>) =
        parser .>. pMany (sep >. parser) |> pMap (fun (h,t) -> h :: t)
        
    let rec pSepBy (sep : Parser<'x>) (parser : Parser<'a>) =
        pAttempt parser |> pBind (fun r ->
            match r with
            | Some h ->
                pAttempt sep |> pBind (fun _ ->
                    pSepBy1 sep parser |> pMap (fun l -> h :: l)
                )
            | None ->
                pSuccess []
        )
        
    let rec pSepByCnt (cnt : int) (sep : Parser<'x>) (parser : Parser<'a>) =
        if cnt = 0 then
            pSuccess []
        else
            parser .>
            sep .>.
            pSepByCnt (cnt - 1) sep parser |>
            pMap (fun (h, t) -> h :: t)
        


    let pRun (input : string) (p : Parser<'a>) =
        match p (Text input) with
        | Some (rest, value) ->
            if Text.IsEmptyOrWhitespace rest then Some value
            else None
        | None ->   
            None

    