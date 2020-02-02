namespace SvgDotNet

open Aardvark.Base
open System.Xml.Linq
open SvgDotNet

[<AutoOpen>]
module private Extensions =
    open Parser
        
    //type ParseState =
    //    {
    //        fontSize    : float
    //        viewOffset  : V2d
    //        viewSize    : V2d
    //        size        : V2d
    //    }

    //module ParseState =
    //    let empty =
    //        {
    //            fontSize = 16.0
    //            viewOffset = V2d.Zero
    //            viewSize = V2d.II
    //            size = V2d.II
    //        }


    let pSep =
        pWhitespace .>
        pAttempt (pRegex [","]) |>
        pMap ignore

    let pBox =
        pWhitespace >.
        TransformParser.pSize .>
        pSep .>.
        TransformParser.pSize .>
        pSep .>.
        TransformParser.pSize .>
        pSep .>.
        TransformParser.pSize |>
        pMap (fun (((x, y), w), h) -> (x,y,w,h))

    let xname str = XName.Get(str)

    type XElement with

        member x.TryGetBox2d(name : string) =
            try
                let a = x.Attribute(xname name)
                if isNull a then None
                else 
                    pBox |> pMap (fun (x,y,w,h) ->
                        Box2d.FromMinAndSize(
                            x.ToPixels(16.0, 1000.0),
                            y.ToPixels(16.0, 1000.0),
                            w.ToPixels(16.0, 1000.0),
                            h.ToPixels(16.0, 1000.0)
                        )
                    ) |> pRun a.Value
            with _ ->
                None

        //member x.TryGetX(name : string) =
        //    try
        //        let a = x.Attribute(xname name)
        //        if isNull a then None
        //        else TransformParser.pSize |> pMap (fun s -> s.ToPixels(state.fontSize, state.viewSize.X)) |> pRun a.Value
        //    with _ ->
        //        None
        //member x.TryGetY(state : ParseState, name : string) =
        //    try
        //        let a = x.Attribute(xname name)
        //        if isNull a then None
        //        else TransformParser.pSize |> pMap (fun s -> s.ToPixels(state.fontSize, state.viewSize.Y))  |> pRun a.Value
        //    with _ ->
        //        None
        //member x.TryGetFloat(name : string) =
        //    try
        //        let a = x.Attribute(xname name)
        //        if isNull a then None
        //        else pFloat |> pRun a.Value
        //    with _ ->
        //        None

        member x.TryGetLength(name : string) =
            try
                let a = x.Attribute(xname name)
                if isNull a then None
                else TransformParser.pSize |> pRun a.Value
            with _ ->
                None
                
        member x.TryGetColor(name : string) =
            try
                let a = x.Attribute(xname name)
                if isNull a then None
                else StyleParser.pColor |> Parser.pRun a.Value
            with _ ->
                None
        member x.TryGetAttribute(name : string) =
            try
                let a = x.Attribute(xname name)
                if isNull a then None
                else Some(a.Value)
            with _ ->
                None
 
        member node.GetProps() =
            
            let style =
                match node.TryGetAttribute("style") with
                | Some s -> 
                    StyleParser.parse s
                | None ->
                    Style.none


            let style = 
                match style.transform with
                | [] ->
                    match node.TryGetAttribute("transform") with
                    | Some t -> 
                        { style with transform = TransformParser.parse t }
                    | None ->
                        style
                | _ ->
                    style


            let style =
                match style.fill with
                | Fill.Unspecified ->
                    match node.TryGetColor "fill" with
                    | Some c -> { style with fill = Fill.Color c }
                    | None -> style
                | _ ->
                    style
                    
            let style =
                match style.stroke with
                | Stroke.Unspecified ->
                    match node.TryGetColor "stroke" with
                    | Some c -> { style with stroke = Stroke.Color c }
                    | None -> style
                | _ ->
                    style

            let id =
                node.TryGetAttribute "id"

            {
                id = id
                style = style
                x = node.TryGetLength("x")
                y = node.TryGetLength("y")
                dx = node.TryGetLength("dx")
                dy = node.TryGetLength("dy")
            }



module SvgParser = 
    open Parser
    let private whitespace = System.Text.RegularExpressions.Regex @"[ \t\r\n]+"

    let private cleanText (text : string) =
        whitespace.Replace(text, " ").Trim()

    let rec private visit (node : XElement) : option<SvgNode> =
        let tag = node.Name.LocalName.ToLower()
        let children = node.Elements() |> Seq.toList
        let props = node.GetProps()


        match tag.ToLower() with
        | "svg" ->
            failwithf "unexpected root element in tree"

        | "g" ->
            let children = children |> List.choose visit
            Some <| { constructor = Group children; props = props }

        | "rect" ->
            match node.TryGetLength("width"), node.TryGetLength("height") with
            | Some w, Some h ->
                let x = defaultArg props.x Length.Zero
                let y = defaultArg props.y Length.Zero

                Some { constructor = Rect(V2L(x,y), V2L(w,h)); props = props }
            | _ ->
                Log.warn "bad rect: %A" node
                None
        | "path" ->
            match node.TryGetAttribute "d" with
            | Some p ->
                let p = 
                    if p.Length > 13 then p.Substring(0, 10) + "..."
                    else p
                Some { constructor = Path []; props = props }
            | None ->
                Log.warn "bad path: %A" node
                None
        | "text" -> 
            let nodes = node.Nodes() |> Seq.toList
            let spans = 
                nodes |> List.choose (fun e ->
                    match e with
                    | :? XElement as e ->
                        if e.Name.LocalName = "tspan" then
                            let props = e.GetProps()
                            Some { 
                                props = props
                                content = cleanText e.Value
                            }
                        else    
                            Log.warn "bad text-span: %A" e
                            None
                    | :? XText as e ->
                        Some { 
                            props = SvgProps.empty
                            content = cleanText e.Value
                        }
                    | e ->
                        Log.warn "bad text-span: %A" e
                        None
                        
                )

            Some { constructor = SvgConstructor.Text spans; props = props }
            
        | "circle" ->
            match node.TryGetLength("r") with
            | Some r ->
                let cx = node.TryGetLength "cx" |> Option.defaultValue Length.Zero
                let cy = node.TryGetLength "cy" |> Option.defaultValue Length.Zero
                Some { constructor = Circle(r, V2L(cx, cy)); props = props }

            | None ->
                None
        | "ellipse" ->
            match node.TryGetLength("rx"), node.TryGetLength("ry") with
            | Some rx, Some ry ->
                let cx = node.TryGetLength "cx" |> Option.defaultValue Length.Zero
                let cy = node.TryGetLength "cy" |> Option.defaultValue Length.Zero
                Some { constructor = Ellipse(rx, ry, V2L(cx, cy)); props = props }
                
            | Some r, None | None, Some r ->
                let cx = node.TryGetLength "cx" |> Option.defaultValue Length.Zero
                let cy = node.TryGetLength "cy" |> Option.defaultValue Length.Zero
                Some { constructor = Ellipse(r, r, V2L(cx, cy)); props = props }

            | _ ->
                None

        | "polyline" ->
            match node.TryGetAttribute "points" with
            | Some pts ->
                let pointSep = pRegex [ @"[ \t]+" ]
                let coordSep = pRegex [ @"[ \t]*,[ \t]*" ]
                let point = TransformParser.pSize .> coordSep .>. TransformParser.pSize |> pMap V2L

                let pPoints = Parser.pSepBy1 pointSep point
                match pPoints |> pRun pts with
                | Some pts -> 
                    Some { constructor = Polyline pts; props = props }
                | None ->
                    None
            | None ->
                None

        | "line" ->
            let x1 = node.TryGetLength "x1" |> Option.defaultValue Length.Zero
            let y1 = node.TryGetLength "y1" |> Option.defaultValue Length.Zero
            let x2 = node.TryGetLength "x2" |> Option.defaultValue Length.Zero
            let y2 = node.TryGetLength "y2" |> Option.defaultValue Length.Zero
            Some { constructor = Line(V2L(x1, y1), V2L(x2, y2)); props = props }

        | "metadata" ->
            None



        | _ ->
            if node.Name.NamespaceName = "http://www.w3.org/2000/svg" then
                Log.warn "bad node: %s" tag
            None


    let rec private visitRoot (node : XElement) =
        let tag = node.Name.LocalName.ToLower()
        let children = node.Elements() |> Seq.toList
        let props = node.GetProps()

        match tag.ToLower() with
        | "svg" ->
            let viewBox = node.TryGetBox2d("viewBox")
            let width = node.TryGetLength("width") |> Option.defaultValue (Length(100.0, SizeUnit.Pixel))
            let height = node.TryGetLength("height") |> Option.defaultValue (Length(100.0, SizeUnit.Pixel))
              
            Some {
                width       = width
                height      = height
                props       = props
                viewBox     = viewBox
                elements    = children |> List.choose visit
            }
        | _ ->
            None

    let tryParse (svg : string) =
        let dom = System.Xml.Linq.XDocument.Parse svg
        visitRoot dom.Root
