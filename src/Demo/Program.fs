open System
open Aardvark.Base
open FSharp.Data.Adaptive
open Aardvark.Base.Rendering
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Application.Slim
open Aardvark.Rendering.Text
open SvgDotNet

type State =
    {
        style : Style
        groupTrafo : Trafo2d
        trafoInGroup : Trafo2d
        trafo : Trafo2d
        width : float
        height : float
        fontSize : float
        viewBox : Box2d
    }

[<AutoOpen>]
module private Helpers =
    type Length with
        member x.X(s : State) = x.ToPixels(s.fontSize, s.viewBox.Size.X)
        member x.Y(s : State) = x.ToPixels(s.fontSize, s.viewBox.Size.Y)
    type V2L with
        member x.ToV2d(s : State) =
            V2d(x.X.X s, x.Y.Y s)
    


module Transform =
    let toTrafo (state : State) (t : Transform) =
        match t with
        | Translate(x, y) ->
            Trafo2d.Translation(x.X state, y.Y state)
        | Rotate(r, None) ->
            Trafo2d.Rotation(-r)
        | Rotate(r, Some(x,y)) ->
            let x = x.X state
            let y = y.Y state
            Trafo2d.Translation(-x,-y) *
            Trafo2d.Rotation(-r) * 
            Trafo2d.Translation(x, y)
        | Skew(x, y) ->
            let mat = M33d(1.0, tan y, 0.0, tan x, 1.0, 0.0, 0.0, 0.0, 1.0)
            Trafo2d(mat, mat.Inverse)
        | Matrix m ->
            let mat = M33d.op_Explicit m
            Trafo2d(mat, mat.Inverse)
        | Scale(x,y) ->
            Trafo2d.Scale(x,y)
        
    let ofStack (state : State) (l : list<Transform>) =
        l |> List.rev |> List.map (toTrafo state) |> List.fold (*) Trafo2d.Identity


module SvgProps =
    let adjustTrafo (isGroup : bool) (state : State) (props : SvgProps) =
        let myTrafo = Transform.ofStack state props.style.transform

        let local2Group = state.trafoInGroup //state.trafo * state.lastGroupTrafo.Inverse

        let abs =
            let getTrafo (dir : V2d) (x : Length) (y : Length) =
                let currentInGroup = local2Group.Forward.TransformPos(V2d.Zero)
                let targetInGroup = V2d(x.X state, y.Y state)
                let delta = dir * (targetInGroup - currentInGroup)
                local2Group *
                Trafo2d.Translation delta *
                local2Group.Inverse

            match props.x, props.y with
            | Some x, Some y -> getTrafo V2d.II x y
            | Some x, None -> getTrafo V2d.IO x Length.Zero
            | None, Some y -> getTrafo V2d.OI Length.Zero y
            | None, None -> Trafo2d.Identity

        let delta = 
            match props.dx, props.dy with
            | Some dx, Some dy -> Trafo2d.Translation(dx.X state, dy.Y state)
            | Some dx, None -> Trafo2d.Translation(dx.X state, 0.0)
            | None, Some dy -> Trafo2d.Translation(0.0, dy.Y state)
            | None, None -> Trafo2d.Identity

        let delta = abs * delta * myTrafo

        if isGroup then
            let mine = delta * state.trafo
            { state with
                trafo = mine
                groupTrafo = mine
                trafoInGroup = Trafo2d.Identity
            }
        else
            { state with
                trafo = delta * state.trafo
                trafoInGroup = delta * state.trafoInGroup
            }


module Stroke = 
    open Aardvark.Rendering.Text

    let rec private tryClip (a : PathSegment) (b : PathSegment) : option<option<PathSegment> * option<PathSegment>> =
        match a, b with
        | Line(a0, a1), Line(b0, b1) ->
            let ra = Ray2d(a0, a1 - a0)
            let rb = Ray2d(b0, b1 - b0)

            let mutable ta = 0.0
            let mutable tb = 0.0
            if ra.Intersects(rb, &ta) && rb.Intersects(ra, &tb) && ta < 1.0 && ta > 0.0 && tb < 1.0 && tb > 0.0 then
                let px = ra.GetPointOnRay(ta)
                Some (PathSegment.tryLine a0 px, PathSegment.tryLine px b1)
            else
                None

        | Line(a1, b1), Arc(alpha0, alpha1, e) ->
            
            //e.GetAlpha


            // intersect line with ellipse and check t/phi! (2 intersections)
            failwith ""
            
        | Line(a1, b1), Bezier2(p0, pc, p1) ->
            // shouldn't be too hard (2 intersections)
            failwith ""

        | Line(a1, b1), Bezier3(p0, c0, c1, p1) ->
            // shouldn't be too hard (??? intersections)
            failwith ""
            
        | Arc(a0, a1, ae), Arc(b0, b1, be) ->
            // shouldn't be too hard (2 intersections)
            failwith ""

        | Arc(a0, a1, ae), Bezier2(b0, b1, b2) ->
            // no idea????
            failwith ""

        | Arc(a0, a1, ae), Bezier3(b0, b1, b2, b3) ->
            // no idea????
            failwith ""

        | Bezier2(a0, a1, a2), Bezier2(b0, b1, b2) ->
            
            failwith ""

        | Bezier2(a0, a1, a2), Bezier3(b0, b1, b2, b3) ->
            failwith ""
            
        | Bezier3(a0, a1, a2, a3), Bezier3(b0, b1, b2, b3) ->
            failwith ""

        | l, r ->
            match tryClip (PathSegment.reverse r) (PathSegment.reverse l) with
            | Some (r', l') ->
                Some (Option.map PathSegment.reverse l', Option.map PathSegment.reverse r')
            | None ->
                None



    let private getCap (style : LineCap) (p : V2d) (d : V2d) (w : float) =
        let n = V2d(-d.Y, d.X)
        
        match style with
        | LineCap.Butt | LineCap.Unspecified | LineCap.Inherit -> 
            [
                PathSegment.tryLine (p - n*w) (p + n*w)
            ]
        | LineCap.Square -> 
            let pStart = p + n*w
            let pEnd = p - n*w
            let p00 = p - n*w - d*w
            let p01 = p + n*w - d*w
            [
                PathSegment.tryLine pEnd p00
                PathSegment.tryLine p00 p01
                PathSegment.tryLine p01 pStart
            ]
        | LineCap.Round ->
            let pStart = p + n*w
            let pEnd = p - n*w
            let p00 = p - d*w
            let p0n = p00 - n*w
            let p0p = p00 + n*w
            [
                PathSegment.tryArcSegment pEnd p0n p00
                PathSegment.tryArcSegment p00 p0p pStart
            ]

    let private offset (w : float) (p : PathSegment) =
        match p with
        | Line(p0, p1) ->
            let d = Vec.normalize (p1 - p0)
            let n = V2d(-d.Y, d.X)
            PathSegment.line (p0+w*n) (p1+w*n)

        | Arc(a, b, e) ->
            let r0 = e.Axis0.Length
            let r1 = e.Axis1.Length
            let d0 = e.Axis0 / r0
            let d1 = e.Axis1 / r1
            PathSegment.arc a b (Ellipse2d(e.Center, d0 * (r0 + w), d1 * (r1 + w)))

        | Bezier2(p0, p1, p2) ->
            let n0 = PathSegment.normal 0.0 p
            let n1 = PathSegment.normal 0.5 p
            let n2 = PathSegment.normal 1.0 p
            PathSegment.bezier2 (p0+w*n0) (p1+w*n1) (p2+w*n2)

        | Bezier3(p0, p1, p2, p3)  ->
            failwith "bezier3 not handled"

    let private joinSegments (style : LineJoin) (miterLimit : float) (a : PathSegment) (b : PathSegment) =
        match tryClip a b with
        | Some(a, b) -> 
            [a], b.Value
        | None ->
            let pa = PathSegment.endPoint a
            let ta = PathSegment.tangent 1.0 a
            let ra = Ray2d(pa, ta)

            let pb = PathSegment.startPoint b
            let tb = PathSegment.tangent 0.0 b
            let rb = Ray2d(pb, -tb)

            let mutable t = 0.0
            if ra.Intersects(rb, &t) && t > 0.0 then
                let px = ra.GetPointOnRay t
                let elements = 
                    match style with
                    | LineJoin.Miter | LineJoin.MiterClip
                    | LineJoin.Inherit | LineJoin.Unspecified | LineJoin.Arcs -> 
                        let theta = acos (clamp -1.0 1.0 (Vec.dot ta -tb))
                        let miterLength = 1.0 / sin (theta / 2.0)
                        if miterLength > miterLimit then
                            [
                                Some a
                                PathSegment.tryLine pa pb
                            ]
                        else
                            [
                                Some a
                                PathSegment.tryLine pa px
                                PathSegment.tryLine px pb
                            ]

                    | LineJoin.Bevel ->
                        [
                            Some a
                            PathSegment.tryLine pa pb
                        ]

                    | LineJoin.Round ->
                        [
                            Some a
                            PathSegment.tryArcSegment pa px pb
                        ]
                            

                elements, b
            else
                [Some a], b





    let outsetPath (distance : float) (cap : LineCap) (join : LineJoin) (miterLimit : float) (path : Path) =
        let input = Path.toArray path
        if input.Length > 0 then
            let first = input.[0]
            let last = input.[input.Length - 1]
            let input = ()

            let segments = System.Collections.Generic.List<PathSegment>()
            
            let add l =
                match l |> List.choose id with
                | [] -> ()
                | l -> segments.AddRange l
            

            let s = distance / 2.0

            add (
                let p = PathSegment.startPoint first
                let d = PathSegment.tangent 0.0 first
                getCap cap p d s
            )

            do
                let comp = Path.toArray path |> Array.map (offset s)
                for i in 0 .. comp.Length - 1 do
                    let c = comp.[i]
                    if i < comp.Length - 1 then 
                        let next = comp.[i+1]
                        let segs, nn = joinSegments join miterLimit c next
                        add segs
                        comp.[i+1] <- nn
                    else
                        add [ Some c ]


            add (
                let p = PathSegment.endPoint last
                let d = -PathSegment.tangent 1.0 last
                getCap cap p d s
            )
                
            do
                let comp = Path.toArray path |> Array.rev |> Array.map (PathSegment.reverse >> offset s)
                for i in 0 .. comp.Length - 1 do
                    let c = comp.[i]
                    if i < comp.Length - 1 then 
                        let next = comp.[i+1]
                        let segs, nn = joinSegments join miterLimit c next
                        add segs
                        comp.[i+1] <- nn
                    else
                        add [ Some c ]

            Path.ofSeq segments

        else
            Path.empty











[<EntryPoint;STAThread>]
let main argv = 
    let readShape (path : string) =
        let content = File.readAllText path
        let test = SvgParser.tryParse content

        let m23 (t : Trafo2d) =
            M23d.op_Explicit t.Forward

        let rec toShapeList (state : State) (n : SvgNode) =
            //let trafo = SvgProps.newTrafo state n.Props
            let style = state.style + n.props.style
            let state = { state with style = style }

            let inline getState (isGroup : bool)  (state : State) (props : SvgProps) =
                let state = 
                    match style.fontSize with
                    | Some size -> { state with fontSize = size.Y state }
                    | _ -> state
                let state = SvgProps.adjustTrafo isGroup state props
                state

            

            match n.constructor with
            | Circle(radius, center) ->
                let state = getState false state n.props
                let center = center.ToV2d state
                let rx = radius.X state
                let ry = radius.Y state
                let ellipse = Ellipse2d(center, V2d.IO * rx, V2d.OI * ry)
                [
                    match style.fill with
                    | Fill.Color color -> 
                        ConcreteShape.fillEllipse color ellipse
                        |> ConcreteShape.transform (m23 state.trafo)
                    | _ ->  
                        ()
                    match style.stroke, style.strokeWidth with
                    | Stroke.Color color, Some len when len.Value > 0.0 ->
                        ConcreteShape.ellipse color len.Value ellipse
                        |> ConcreteShape.transform (m23 state.trafo)
                    | _ ->  
                        ()
                ]
            | Ellipse(rx, ry, center) ->
                let state = getState false state n.props
                let center = center.ToV2d state
                let rx = rx.X state
                let ry = ry.Y state
                let ellipse = Ellipse2d(center, V2d.IO * rx, V2d.OI * ry)
                [
                    match style.fill with
                    | Fill.Color color -> 
                        ConcreteShape.fillEllipse color ellipse
                        |> ConcreteShape.transform (m23 state.trafo)
                    | _ ->  
                        ()
                    match style.stroke, style.strokeWidth with
                    | Stroke.Color color, Some len when len.Value > 0.0 ->
                        ConcreteShape.ellipse color len.Value ellipse
                        |> ConcreteShape.transform (m23 state.trafo)
                    | _ ->  
                        ()
                ]

            | Line(p0, p1) ->
                match style.stroke, style.strokeWidth with
                | Stroke.Color color, Some w when w.Value > 0.0 ->
                    let p0 = p0.ToV2d state
                    let p1 = p1.ToV2d state

                    let c = Vec.normalize (p1 - p0)
                    let n = V2d(-c.Y, c.X)
                    let pc = 0.5 * (p0 + p1) + n * 20.0

                    let path =
                        Path.ofList [
                            PathSegment.line p0 pc
                            PathSegment.line pc p1
                            
                        ]

                    let miterLimit = defaultArg style.miterLimit 4.0
                    let path = Stroke.outsetPath w.Value style.lineCap style.lineJoin miterLimit path
                    [ ConcreteShape.ofPath state.trafo.Forward color path ]


                    //let dir = p1 - p0
                    //let len = Vec.length dir
                    //let x = dir / len
                    //let y = V2d(-x.Y, x.X)
                    //let wHalf = w.Value / 2.0
                    //let transform =
                    //    state.trafo.Forward *
                    //    M33d.FromCols(V3d(x, 0.0), V3d(y, 0.0), V3d(p0, 1.0))


                    //let shape = 
                    //    match style.lineCap with
                    //    | LineCap.Unspecified | LineCap.Inherit | LineCap.Butt ->
                    //        ConcreteShape.fillRectangle color (Box2d(0.0, -wHalf, len, wHalf))
                    //    | LineCap.Square ->
                    //        ConcreteShape.fillRectangle color (Box2d(-wHalf, -wHalf, len + wHalf, wHalf))
                    //    | LineCap.Round ->
                    //        ConcreteShape.fillRoundedRectangle color wHalf (Box2d(-wHalf, -wHalf, len + wHalf, wHalf))

                    //[ ConcreteShape.transform (M23d.op_Explicit transform) shape ]
                    
                | _ ->
                    []

            | Polyline pts ->
                let state = getState false state n.props
                let pts = pts |> List.map (fun p -> p.ToV2d state)
                match style.stroke, style.strokeWidth with
                | Stroke.Color color, Some w when w.Value > 0.0 ->

                    let rec traverse (last : option<V2d>) (p : list<V2d>) =
                        match p with
                        | [] -> []
                        | p :: rest ->
                            match last with
                            | Some l ->
                                match PathSegment.tryLine l p with
                                | Some l -> l :: traverse (Some p) rest
                                | None -> traverse (Some p) rest
                            | None ->
                                traverse (Some p) rest


                    let miterLimit = defaultArg style.miterLimit 4.0
                    let path = Path.ofList (traverse None pts)
                    let path = Stroke.outsetPath w.Value style.lineCap style.lineJoin miterLimit path
                    [ ConcreteShape.ofPath state.trafo.Forward color path ]
                | _ ->
                    []
            | Polygon _  ->
                []

            | Rect(_, size) ->  
                let state = getState false state n.props
                let size = V2d(size.X.X state, size.Y.Y state)
                [
                    match style.fill with
                    | Fill.Color color -> 
                        ConcreteShape.fillRectangle color (Box2d.FromMinAndSize(V2d.Zero, size))
                        |> ConcreteShape.transform (m23 state.trafo)
                    | _ ->  
                        ()
                    match style.stroke, style.strokeWidth with
                    | Stroke.Color color, Some len when len.Value > 0.0 ->
                        ConcreteShape.rectangle color len.Value (Box2d.FromMinAndSize(V2d.Zero, size))
                        |> ConcreteShape.transform (m23 state.trafo)
                    | _ ->  
                        ()
                ]

            | Group(children) ->
                let state = getState true state n.props
                children |> List.collect (fun c ->
                    toShapeList state c
                )
            | Text(spans) ->
                let parentState = getState false state n.props

                let mutable state = parentState
                spans |> List.collect (fun s ->
                    let style = style + s.props.style
                    state <- getState false { state with style = style } s.props

                    let font =
                        match style.fontFamily with
                        | Some family -> 
                            try Font family
                            with _ -> Font "Times New Roman"
                        | None -> Font "Times New Roman"

                    let fontSize =
                        match style.fontSize with
                        | Some len -> len.ToPixels(state.fontSize, state.viewBox.SizeY)
                        | None -> 16.0
                    
                    let letterSpacing =
                        match style.letterSpacing with
                        | Some s -> s.ToPixels(fontSize, state.viewBox.SizeY)
                        | None -> 0.0

                    let wordSpacing =
                        match style.wordSpacing with
                        | Some s -> s.ToPixels(fontSize, state.viewBox.SizeY)
                        | None -> 0.0

                    let color =
                        match style.fill with
                        | Fill.Color c -> c
                        | _ -> C4b.Black
                    
                    let cfg =
                        {
                            font = font
                            TextConfig.align = TextAlignment.Left
                            TextConfig.color = color
                            TextConfig.flipViewDependent = false
                            TextConfig.renderStyle = RenderStyle.Normal
                        }


                    let shapes =
                        let list = cfg.Layout s.content
                        let renderTrafo =   
                            let m4 = list.renderTrafo.Forward
                            let m = M33d.FromRows(m4.R0.XYW, m4.R1.XYW, V3d.OOI)
                            Trafo2d(m, m.Inverse)
                            
                        let trans =
                            Trafo2d.Translation(-list.bounds.Min.X, 0.0) 

                        let trafo =
                            renderTrafo * trans * Trafo2d.Scale(fontSize, -fontSize)

                        list.concreteShapes |> List.map (fun s ->
                            s |> ConcreteShape.transform (m23 trafo)
                        )


                    let shapes =
                        let rec adjust (offset : float) (str : string) (idx : int) (g : list<ConcreteShape>) =
                            if idx >= str.Length then
                                let t = Trafo2d.Translation(offset, 0.0) |> m23
                                g |> List.map (ConcreteShape.transform t)
                            else
                                let c = str.[idx]
                                Log.warn "character %A" c
                                match c with
                                | ' ' ->
                                    adjust (offset + wordSpacing + letterSpacing) str (idx + 1) g
                                | '\t' ->
                                    adjust (offset + 4.0 * (wordSpacing + letterSpacing)) str (idx + 1) g
                                | '\r' ->
                                    adjust offset str (idx + 1) g
                                | '\n' ->
                                    adjust 0.0 str (idx + 1) g
                                | _ ->
                                    match g with
                                    | g :: rest ->
                                        let t = Trafo2d.Translation(offset, 0.0) |> m23
                                        ConcreteShape.transform t g :: adjust (offset + letterSpacing) str (idx + 1) rest
                                    | [] ->
                                        []
                                    
                                    
                        adjust 0.0 s.content 0 shapes
                        
                    let bounds = shapes |> Seq.collect (fun s -> s.bounds.ComputeCorners() :> seq<_>) |> Box2d
                    let offset = bounds.Max.X

                    let whiteSpaceSize = cfg.font.Spacing * fontSize
                    let advance = whiteSpaceSize + letterSpacing * fontSize

                    let glyphTrafo = state.trafo

                    let trans = Trafo2d.Translation(offset + advance * 1.25, 0.0)
                    state <- 
                        { state with 
                            trafo = trans * state.trafo 
                            trafoInGroup = trans * state.trafoInGroup
                        }

                    shapes |> List.map (fun s ->
                        s |> ConcreteShape.transform (m23 glyphTrafo)
                    )

                )
            | Path(_) ->
                []
    
        let rootToShapeList (svg : Svg) =
            let viewBox = 
                match svg.viewBox with
                | Some b -> b
                | None -> Box2d.FromMinAndSize(0.0, 0.0, 100.0, 100.0)

            let state = 
                {
                    viewBox = viewBox
                    width = svg.width.ToPixels(16.0, viewBox.Size.X)
                    height = svg.height.ToPixels(16.0, viewBox.Size.Y)
                    trafo = Trafo2d.Identity
                    groupTrafo = Trafo2d.Identity
                    trafoInGroup = Trafo2d.Identity
                    style = Style.initial
                    fontSize = 16.0
                }

            svg.elements |> List.collect (fun c ->
                toShapeList state c
            )

        match test with
        | Some svg ->
            let shapes = 
                svg 
                |> rootToShapeList
                |> ShapeList.ofList
            { shapes with flipViewDependent = true }

        | None ->
            ShapeList.ofList []



    let path = @"C:\Users\Schorsch\Downloads\Example_svg.svg"

    let shapes = readShape path |> cval



    Ag.initialize()
    Aardvark.Init()

    use app = new OpenGlApplication()
    use win = app.CreateGameWindow(8)


    let initialView = CameraView.lookAt (V3d(0,6,0)) V3d.Zero V3d.OOI
    let view = initialView |> DefaultCameraController.control win.Mouse win.Keyboard win.Time
    let proj = win.Sizes |> AVal.map (fun s -> Frustum.perspective 60.0 0.1 100.0 (float s.X / float s.Y))

    let trafo = 
        shapes |> AVal.map (fun shapes ->
            let bb = shapes.bounds
            let off = V3d(-bb.Center.XY, 0.0)
            let scale = 4.0 / bb.Size.NormMax
            Trafo3d.Translation off *
            Trafo3d.Scale(scale, -scale, 1.0)
        )

    let fill = cval FillMode.Fill

    win.Keyboard.DownWithRepeats.Values.Add (fun k ->
        match k with
        | Keys.Space ->
            transact (fun () ->
                fill.Value <-
                    match fill.Value with
                    | FillMode.Fill -> FillMode.Line
                    | _ -> FillMode.Fill
            )

        | Keys.Enter | Keys.R | Keys.F5 ->
            transact (fun () ->
                shapes.Value <- readShape path
            )

        | _ ->
            ()
    )
   
    let sg =
        Sg.shape shapes
        |> Sg.trafo trafo
        |> Sg.transform (Trafo3d.RotationX(Constant.PiHalf))
        //|> Sg.projTrafo (win.Sizes |> AVal.map (fun s -> Trafo3d.Scale(float s.Y / float s.X, 1.0, 1.0)))
        |> Sg.fillMode fill
        |> Sg.viewTrafo (view |> AVal.map CameraView.viewTrafo)
        |> Sg.projTrafo (proj |> AVal.map Frustum.projTrafo)

    let task =
        RenderTask.ofList [
            app.Runtime.CompileClear(win.FramebufferSignature, AVal.constant C4f.Gray80)
            app.Runtime.CompileRender(win.FramebufferSignature, sg)
        ]
    win.RenderTask <- task
    win.Run()
    0
