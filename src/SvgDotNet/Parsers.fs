namespace SvgDotNet

open Aardvark.Base
open SvgDotNet



module TransformParser =
    open Parser
    
    let pSep = pRegex [ "(?:[ \t]+|(?:[ \t]*,))" ] |> pMap (fun _ -> ())

    let pSizeUnit =
        pAnyString [
            "px", SizeUnit.Pixel
            "%", SizeUnit.Percent
            "em", SizeUnit.EM
            "pt", SizeUnit.Point
            "vw", SizeUnit.ViewWidth
            "vh", SizeUnit.ViewHeight
            "mm", SizeUnit.Millimeter
            "cm", SizeUnit.Centimeter
            "m", SizeUnit.Meter
            "in", SizeUnit.Inch
            "ft", SizeUnit.Feet
        ]
        
    let pSizeOrFloat =
        pFloat .>.
        pAttempt (
            pWhitespace >.
            pSizeUnit
        )

    let pSize =
        pSizeOrFloat |> pMap (fun (v, u) -> Length(v, defaultArg u SizeUnit.Pixel))

    let pAngleUnit =
        pRegex [ "[ \t]*(deg|rad|grad|turn)"] |> pMap (fun m ->
            match m.Groups.[1].Value with
            | "deg" -> 1.0
            | "rad" -> Constant.DegreesPerRadian
            | "grad" -> Constant.DegreesPerGon
            | "turn" -> 360.0
            | _ -> 0.0
        )

    let pAngle =
        pRegex [ @"[ \t]*" ] >.
        pFloat .>.
        pAttempt pAngleUnit |>
        pMap (fun (v, u) -> v * defaultArg u 1.0)

    let pValue =
        pChoice [
            pSizeOrFloat |> pMap (fun (v,u) ->
                match u with
                | None -> Float v
                | Some u -> Length(v,u) |> Size
            )
            pAngle                          |> pMap Angle
            pRegex [ @"[a-zA-Z_\-][a-zA-Z_\-0-9]*" ] |> pMap (fun m -> Identifier m.Value)
            pRegex [ @"[^:; ]+"] |> pMap (fun m -> Unknown m.Value)
        ]

    let pCall =
        pRegex [ @"[ \t]*([a-zA-Z_\-][a-zA-Z_\-0-9]*)[ \t]*\(" ] .>.
        pSepBy1 pSep pValue .>
        pRegex [@"[ \t]*\)"] |>
        pMap (fun (name, sizes) -> name.Groups.[1].Value, sizes)

    let (|L|_|) (v : Value) =
        match v with
        | Float v -> Some (Length(v, SizeUnit.Pixel))
        | Size s -> Some s
        | _ -> None

    let (|F|_|) (v : Value) =
        match v with
        | Float v -> Some v
        | Size s -> Some(s.ToPixels(16.0, 16.0))
        | _ -> None

    let pTransform =
        pCall |> pMap (fun (name, args) ->
            match name, args with
            | "translate", [L x; L y] -> Translate(x,y) |> Some
            | "translate", [L x] -> Translate(x, Length.Zero) |> Some
            | "translateX", [L x] -> Translate(x, Length.Zero) |> Some
            | "translateY", [L y] -> Translate(Length.Zero, y) |> Some
            | "rotate", [F deg] -> Rotate(Constant.RadiansPerDegree * deg, None) |> Some
            | "rotate", [F deg; L cx; L cy] -> Rotate(Constant.RadiansPerDegree * deg, Some (cx,cy)) |> Some
            | "scale", [F f] -> Scale(f,f) |> Some
            | "scale", [F x; F y] -> Scale(x,y) |> Some
            | "skewX", [F deg] -> Skew(Constant.RadiansPerDegree * deg, 0.0) |> Some
            | "skewY", [F deg] -> Skew(0.0, Constant.RadiansPerDegree * deg) |> Some
            | "matrix", [F a; F b; F c; F d; F e; F f] -> M23d(a, c, e, b, d, f) |> Matrix |> Some
            | name, args -> 
                Log.warn "bad transformation: %s(%A)" name args
                None
        )

    let pTransforms =
        pSepBy1 pSep pTransform |>
        pMap (List.choose id)

    let parse (str : string) =
        let str = Text str
        match pTransforms str with
        | Some(str, ts) ->
            if Text.IsEmptyOrWhitespace str then   
                ts
            else
                failwithf "bad trafo: %A" str
        | None ->
            failwithf "bad trafo: %A" str
                
module StyleParser =
    open Parser
    open TransformParser
    open System.Globalization



    let pSep = pRegex [ "(?:[ \t]+|(?:[ \t]*,))" ] |> pMap (fun _ -> ())

    let pHexByte =
        pRegex [ "[ \t]*([0-9a-fA-F][0-9a-fA-F])"] |> 
        pMap (fun m -> System.Int32.Parse(m.Groups.[1].Value, NumberStyles.HexNumber) |> byte)
        
    let pByte =
        pInt |> pMap byte

    let pHexColor =
        pRegex [ "[ \t]*#"] >.
        pHexByte .>. pHexByte .>. pHexByte |>
        pMap (fun ((r,g),b) -> C4b(r,g,b))
    
    let pRgbColor =
        pRegex [@"[ \t]*rgb\("] >.
        pByte .>
        pSep .>.
        pByte .>
        pSep .>.
        pByte .>
        pRegex [@"[ \t]*\)"] |>
        pMap (fun ((r,g),b) -> C4b(r,g,b))

    let pRgbaColor =
        pRegex [@"[ \t]*rgba\("] >.
        pByte .>
        pSep .>.
        pByte .>
        pSep .>.
        pByte .>
        pSep .>.
        pFloat .>
        pRegex [@"[ \t]*\)"] |>
        pMap (fun (((r,g),b),a) -> C4b(r,g,b, byte (255.0 * a)))
        

    let pNamedColor : Parser<C4b> =
        pAnyString [
            "aliceblue",            C4b(0xF0uy, 0xF8uy, 0xFFuy)
            "antiquewhite",         C4b(0xFAuy, 0xEBuy, 0xD7uy)
            "aqua",                 C4b.Cyan
            "aquamarine",           C4b(0x7Fuy, 0xFFuy, 0xD4uy)
            "azure",                C4b(0xF0uy, 0xFFuy, 0xFFuy)
            "beige",                C4b(0xF5uy, 0xF5uy, 0xDCuy)
            "bisque",               C4b(0xFFuy, 0xE4uy, 0xC4uy)
            "black",                C4b.Black
            "blanchedalmond",       C4b(0xFFuy, 0xEBuy, 0xCDuy)
            "blue",                 C4b.Blue
            "blueviolet",           C4b(0x8Auy, 0x2Buy, 0xE2uy)
            "brown",                C4b(0xA5uy, 0x2Auy, 0x2Auy)
            "burlywood",            C4b(0xDEuy, 0xB8uy, 0x87uy)
            "cadetblue",            C4b(0x5Fuy, 0x9Euy, 0xA0uy)
            "chartreuse",           C4b(0x7Fuy, 0xFFuy, 0x00uy)
            "chocolate",            C4b(0xD2uy, 0x69uy, 0x1Euy)
            "coral",                C4b(0xFFuy, 0x7Fuy, 0x50uy)
            "cornflowerblue",       C4b(0x64uy, 0x95uy, 0xEDuy)
            "cornsilk",             C4b(0xFFuy, 0xF8uy, 0xDCuy)
            "crimson",              C4b(0xDCuy, 0x14uy, 0x3Cuy)
            "cyan",                 C4b.Cyan
            "darkblue",             C4b(0x00uy, 0x00uy, 0x8Buy)
            "darkcyan",             C4b(0x00uy, 0x8Buy, 0x8Buy)
            "darkgoldenrod",        C4b(0xB8uy, 0x86uy, 0x0Buy)
            "darkgray",             C4b(0xA9uy, 0xA9uy, 0xA9uy)
            "darkgreen",            C4b(0x00uy, 0x64uy, 0x00uy)
            "darkgrey",             C4b(0xA9uy, 0xA9uy, 0xA9uy)
            "darkkhaki",            C4b(0xBDuy, 0xB7uy, 0x6Buy)
            "darkmagenta",          C4b(0x8Buy, 0x00uy, 0x8Buy)
            "darkolivegreen",       C4b(0x55uy, 0x6Buy, 0x2Fuy)
            "darkorange",           C4b(0xFFuy, 0x8Cuy, 0x00uy)
            "darkorchid",           C4b(0x99uy, 0x32uy, 0xCCuy)
            "darkred",              C4b(0x8Buy, 0x00uy, 0x00uy)
            "darksalmon",           C4b(0xE9uy, 0x96uy, 0x7Auy)
            "darkseagreen",         C4b(0x8Fuy, 0xBCuy, 0x8Fuy)
            "darkslateblue",        C4b(0x48uy, 0x3Duy, 0x8Buy)
            "darkslategray",        C4b(0x2Fuy, 0x4Fuy, 0x4Fuy)
            "darkslategrey",        C4b(0x2Fuy, 0x4Fuy, 0x4Fuy)
            "darkturquoise",        C4b(0x00uy, 0xCEuy, 0xD1uy)
            "darkviolet",           C4b(0x94uy, 0x00uy, 0xD3uy)
            "deeppink",             C4b(0xFFuy, 0x14uy, 0x93uy)
            "deepskyblue",          C4b(0x00uy, 0xBFuy, 0xFFuy)
            "dimgray",              C4b(0x69uy, 0x69uy, 0x69uy)
            "dimgrey",              C4b(0x69uy, 0x69uy, 0x69uy)
            "dodgerblue",           C4b(0x1Euy, 0x90uy, 0xFFuy)
            "firebrick",            C4b(0xB2uy, 0x22uy, 0x22uy)
            "floralwhite",          C4b(0xFFuy, 0xFAuy, 0xF0uy)
            "forestgreen",          C4b(0x22uy, 0x8Buy, 0x22uy)
            "fuchsia",              C4b.Magenta
            "gainsboro",            C4b(0xDCuy, 0xDCuy, 0xDCuy)
            "ghostwhite",           C4b(0xF8uy, 0xF8uy, 0xFFuy)
            "goldenrod",            C4b(0xDAuy, 0xA5uy, 0x20uy)
            "gold",                 C4b(0xFFuy, 0xD7uy, 0x00uy)
            "gray",                 C4b(0x80uy, 0x80uy, 0x80uy)
            "green",                C4b(0x00uy, 0x80uy, 0x00uy)
            "greenyellow",          C4b(0xADuy, 0xFFuy, 0x2Fuy)
            "grey",                 C4b(0x80uy, 0x80uy, 0x80uy)
            "honeydew",             C4b(0xF0uy, 0xFFuy, 0xF0uy)
            "hotpink",              C4b(0xFFuy, 0x69uy, 0xB4uy)
            "indianred",            C4b(0xCDuy, 0x5Cuy, 0x5Cuy)
            "indigo",               C4b(0x4Buy, 0x00uy, 0x82uy)
            "ivory",                C4b(0xFFuy, 0xFFuy, 0xF0uy)
            "khaki",                C4b(0xF0uy, 0xE6uy, 0x8Cuy)
            "lavenderblush",        C4b(0xFFuy, 0xF0uy, 0xF5uy)
            "lavender",             C4b(0xE6uy, 0xE6uy, 0xFAuy)
            "lawngreen",            C4b(0x7Cuy, 0xFCuy, 0x00uy)
            "lemonchiffon",         C4b(0xFFuy, 0xFAuy, 0xCDuy)
            "lightblue",            C4b(0xADuy, 0xD8uy, 0xE6uy)
            "lightcoral",           C4b(0xF0uy, 0x80uy, 0x80uy)
            "lightcyan",            C4b(0xE0uy, 0xFFuy, 0xFFuy)
            "lightgoldenrodyellow", C4b(0xFAuy, 0xFAuy, 0xD2uy)
            "lightgray",            C4b(0xD3uy, 0xD3uy, 0xD3uy)
            "lightgreen",           C4b(0x90uy, 0xEEuy, 0x90uy)
            "lightgrey",            C4b(0xD3uy, 0xD3uy, 0xD3uy)
            "lightpink",            C4b(0xFFuy, 0xB6uy, 0xC1uy)
            "lightsalmon",          C4b(0xFFuy, 0xA0uy, 0x7Auy)
            "lightseagreen",        C4b(0x20uy, 0xB2uy, 0xAAuy)
            "lightskyblue",         C4b(0x87uy, 0xCEuy, 0xFAuy)
            "lightslategray",       C4b(0x77uy, 0x88uy, 0x99uy)
            "lightslategrey",       C4b(0x77uy, 0x88uy, 0x99uy)
            "lightsteelblue",       C4b(0xB0uy, 0xC4uy, 0xDEuy)
            "lightyellow",          C4b(0xFFuy, 0xFFuy, 0xE0uy)
            "lime",                 C4b.Green
            "limegreen",            C4b(0x32uy, 0xCDuy, 0x32uy)
            "linen",                C4b(0xFAuy, 0xF0uy, 0xE6uy)
            "magenta",              C4b.Magenta
            "maroon",               C4b(0x80uy, 0x00uy, 0x00uy)
            "mediumaquamarine",     C4b(0x66uy, 0xCDuy, 0xAAuy)
            "mediumblue",           C4b(0x00uy, 0x00uy, 0xCDuy)
            "mediumorchid",         C4b(0xBAuy, 0x55uy, 0xD3uy)
            "mediumpurple",         C4b(0x93uy, 0x70uy, 0xDBuy)
            "mediumseagreen",       C4b(0x3Cuy, 0xB3uy, 0x71uy)
            "mediumslateblue",      C4b(0x7Buy, 0x68uy, 0xEEuy)
            "mediumspringgreen",    C4b(0x00uy, 0xFAuy, 0x9Auy)
            "mediumturquoise",      C4b(0x48uy, 0xD1uy, 0xCCuy)
            "mediumvioletred",      C4b(0xC7uy, 0x15uy, 0x85uy)
            "midnightblue",         C4b(0x19uy, 0x19uy, 0x70uy)
            "mintcream",            C4b(0xF5uy, 0xFFuy, 0xFAuy)
            "mistyrose",            C4b(0xFFuy, 0xE4uy, 0xE1uy)
            "moccasin",             C4b(0xFFuy, 0xE4uy, 0xB5uy)
            "navajowhite",          C4b(0xFFuy, 0xDEuy, 0xADuy)
            "navy",                 C4b(0x00uy, 0x00uy, 0x80uy)
            "oldlace",              C4b(0xFDuy, 0xF5uy, 0xE6uy)
            "olive",                C4b(0x80uy, 0x80uy, 0x00uy)
            "olivedrab",            C4b(0x6Buy, 0x8Euy, 0x23uy)
            "orange",               C4b(0xFFuy, 0xA5uy, 0x00uy)
            "orangered",            C4b(0xFFuy, 0x45uy, 0x00uy)
            "orchid",               C4b(0xDAuy, 0x70uy, 0xD6uy)
            "palegoldenrod",        C4b(0xEEuy, 0xE8uy, 0xAAuy)
            "palegreen",            C4b(0x98uy, 0xFBuy, 0x98uy)
            "paleturquoise",        C4b(0xAFuy, 0xEEuy, 0xEEuy)
            "palevioletred",        C4b(0xDBuy, 0x70uy, 0x93uy)
            "papayawhip",           C4b(0xFFuy, 0xEFuy, 0xD5uy)
            "peachpuff",            C4b(0xFFuy, 0xDAuy, 0xB9uy)
            "peru",                 C4b(0xCDuy, 0x85uy, 0x3Fuy)
            "pink",                 C4b(0xFFuy, 0xC0uy, 0xCBuy)
            "plum",                 C4b(0xDDuy, 0xA0uy, 0xDDuy)
            "powderblue",           C4b(0xB0uy, 0xE0uy, 0xE6uy)
            "purple",               C4b(0x80uy, 0x00uy, 0x80uy)
            "rebeccapurple",        C4b(0x66uy, 0x33uy, 0x99uy)
            "red",                  C4b.Red
            "rosybrown",            C4b(0xBCuy, 0x8Fuy, 0x8Fuy)
            "royalblue",            C4b(0x41uy, 0x69uy, 0xE1uy)
            "saddlebrown",          C4b(0x8Buy, 0x45uy, 0x13uy)
            "salmon",               C4b(0xFAuy, 0x80uy, 0x72uy)
            "sandybrown",           C4b(0xF4uy, 0xA4uy, 0x60uy)
            "seagreen",             C4b(0x2Euy, 0x8Buy, 0x57uy)
            "seashell",             C4b(0xFFuy, 0xF5uy, 0xEEuy)
            "sienna",               C4b(0xA0uy, 0x52uy, 0x2Duy)
            "silver",               C4b(0xC0uy, 0xC0uy, 0xC0uy)
            "skyblue",              C4b(0x87uy, 0xCEuy, 0xEBuy)
            "slateblue",            C4b(0x6Auy, 0x5Auy, 0xCDuy)
            "slategray",            C4b(0x70uy, 0x80uy, 0x90uy)
            "slategrey",            C4b(0x70uy, 0x80uy, 0x90uy)
            "snow",                 C4b(0xFFuy, 0xFAuy, 0xFAuy)
            "springgreen",          C4b(0x00uy, 0xFFuy, 0x7Fuy)
            "steelblue",            C4b(0x46uy, 0x82uy, 0xB4uy)
            "tan",                  C4b(0xD2uy, 0xB4uy, 0x8Cuy)
            "teal",                 C4b(0x00uy, 0x80uy, 0x80uy)
            "thistle",              C4b(0xD8uy, 0xBFuy, 0xD8uy)
            "tomato",               C4b(0xFFuy, 0x63uy, 0x47uy)
            "turquoise",            C4b(0x40uy, 0xE0uy, 0xD0uy)
            "violet",               C4b(0xEEuy, 0x82uy, 0xEEuy)
            "wheat",                C4b(0xF5uy, 0xDEuy, 0xB3uy)
            "white",                C4b.White
            "whitesmoke",           C4b(0xF5uy, 0xF5uy, 0xF5uy)
            "yellow",               C4b.Yellow
            "yellowgreen",          C4b(0x9Auy, 0xCDuy, 0x32uy)
        ] 

    let pColor =
        pChoice [
            pHexColor
            pRgbColor
            pRgbaColor
            pNamedColor
        ]



    let pValue =
        pChoice [
            pColor                          |> pMap Color
            pSizeOrFloat |> pMap (fun (v,u) ->
                match u with
                | None -> Float v
                | Some u -> Length(v,u) |> Size
            )
            pTransforms                     |> pMap Transform
            pRegex [ @"[a-zA-Z_\-][a-zA-Z_\-0-9]*" ] |> pMap (fun m -> Identifier m.Value)
            pRegex [ @"[^:; ]+"] |> pMap (fun m -> Unknown m.Value)
        ]

    let pStyleProperty =
        pRegex [@"[ \t]*([a-zA-Z_\-][a-zA-Z_\-0-9]*)[ \t]*\:"] |>
        pMap (fun m -> m.Groups.[1].Value) .>.
        pMany (pValue .> pWhitespace) .>
        pRegex [@";?"]

    let pStyleProperties  =
        pMany pStyleProperty |> 
        pMap (List.filter (fun (_,v) -> not (List.isEmpty v))) |>
        pMap Map.ofList

    let pStyle = 
        pStyleProperties |>
        pMap Style.ofMap


    //let private printNamedColors (file : string) =
    //    let data = System.IO.File.ReadAllText file
    //    let d = System.Text.Json.JsonDocument.Parse data
    //    let all = 
    //        d.RootElement.EnumerateObject()
    //        |> Seq.toList 
    //        |> List.map (fun kv ->
    //            let name = kv.Name
    //            let value = pHexColor |> pRun (kv.Value.GetString())
    //            match value with
    //            | Some v -> 
    //                name, v
    //                //printfn "\"%s\",C4b(%duy, %duy, %duy, 255uy)" name v.R v.G v.B
    //            | None ->
    //                name, C4b.White
    //                //printfn "\"%s\", C4b.White" name
    //        )

        //let longestName = all |> Seq.map (fun (n,_) -> n.Length) |> Seq.max
        //let padding (str : string) : string = 
        //    System.String(' ', longestName - str.Length)

        //let namedColors = 
        //    typeof<C4b>.GetFields(System.Reflection.BindingFlags.Static ||| System.Reflection.BindingFlags.Public)
        //    |> Array.filter (fun f -> f.FieldType = typeof<C4b>)
        //    |> Array.map (fun f ->
        //        let v = f.GetValue(null) |> unbox<C4b>
        //        v, f.Name
        //    )
        //    |> HMap.ofArray

        //for (name, color) in all do
        //    let p = padding name
        //    match HMap.tryFind color namedColors with
        //    | Some aardName ->
        //        printfn "            \"%s\", %sC4b.%s" name p aardName
        //    | None ->   
        //        printfn "            \"%s\", %sC4b(0x%02Xuy, 0x%02Xuy, 0x%02Xuy)" name p color.R color.G color.B
     
    let parse (style : string) =
        match pStyle |> pRun style with
        | Some props -> props
        | None -> Style.none
