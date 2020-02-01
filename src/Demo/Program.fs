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

[<EntryPoint;STAThread>]
let main argv = 
    let readShape (path : string) =
        let content = File.readAllText path
        let test = SvgParser.tryParse content

        let m23 (t : Trafo2d) =
            M23d.op_Explicit t.Forward

        let rec toShapeList (state : State) (n : SvgNode) =
            //let trafo = SvgProps.newTrafo state n.Props
            let style = state.style + n.Props.style
            let state = { state with style = style }

            let inline getState (isGroup : bool)  (state : State) (props : SvgProps) =
                let state = 
                    match style.fontSize with
                    | Some size -> { state with fontSize = size.Y state }
                    | _ -> state
                let state = SvgProps.adjustTrafo isGroup state props
                state

            

            match n with
            | Rect(_, w, h) ->  
                let state = getState false state n.Props
                let size = V2d(w.X state, h.Y state)
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

            | Group(_, children) ->
                let state = getState true state n.Props
                children |> List.collect (fun c ->
                    toShapeList state c
                )
            | Text(_, spans) ->
                let parentState = getState false state n.Props

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
                        | Some s -> s.ToPixels(fontSize, state.viewBox.SizeY) / fontSize
                        | None -> 0.0

                    let wordSpacing =
                        match style.wordSpacing with
                        | Some s -> s.ToPixels(fontSize, state.viewBox.SizeY) / fontSize
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
            | Path(_, _) ->
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
