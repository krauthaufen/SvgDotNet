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

            let x =
                match node.TryGetLength("x") with
                | Some x -> x
                | None -> Length.Zero
            let y = 
                match node.TryGetLength("y") with
                | Some y -> y
                | None -> Length.Zero
            
            let id =
                node.TryGetAttribute "id"

            {
                id = id
                style = style
                x = x
                y = y
            }



module SvgParser = 
    let rec private visit (node : XElement) : option<SvgNode> =
        let tag = node.Name.LocalName.ToLower()
        let children = node.Elements() |> Seq.toList
        let props = node.GetProps()


        match tag.ToLower() with
        | "svg" ->
            failwithf "unexpected root element in tree"

        | "g" ->
            let children = children |> List.choose visit
            Some <| Group(props, children)

        | "rect" ->
            match node.TryGetLength("width"), node.TryGetLength("height") with
            | Some w, Some h ->
                Some (Rect(props, w, h))
            | _ ->
                Log.warn "bad rect: %A" node
                None
        | "path" ->
            match node.TryGetAttribute "d" with
            | Some p ->
                let p = 
                    if p.Length > 13 then p.Substring(0, 10) + "..."
                    else p
                Some (Path(props, p))
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
                                content = e.Value.Trim [| ' '; '\t'; '\r'; '\n' |]
                            }
                        else    
                            Log.warn "bad text-span: %A" e
                            None
                    | :? XText as e ->
                        Some { 
                            props = SvgProps.empty
                            content = e.Value.Trim [| ' '; '\t'; '\r'; '\n' |]
                        }
                    | e ->
                        Log.warn "bad text-span: %A" e
                        None
                        
                )

            Some (SvgNode.Text(props, spans))
            
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
