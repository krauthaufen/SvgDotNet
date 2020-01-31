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
    let trafo (state : State) (props : SvgProps) =
        let trans = Trafo2d.Translation(props.x.X state, props.y.Y state)
        Transform.ofStack state props.style.transform * 
        trans

[<EntryPoint;STAThread>]
let main argv = 
    let readShape (path : string) =
        let content = File.readAllText path
        let test = SvgParser.tryParse content

        let m23 (t : Trafo2d) =
            M23d.op_Explicit t.Forward

        let rec toShapeList (state : State) (n : SvgNode) =
            let trafo = 
                SvgProps.trafo state n.Props *
                state.trafo

            let style = state.style + n.Props.style

            let state =
                match style.fontSize with
                | Some s -> { state with fontSize = s.Y state }
                | _ -> state

            let state =
                { state with trafo = trafo }

            match n with
            | Rect(_, w, h) ->  
                let size = V2d(w.X state, h.Y state)
                [
                    match style.fill with
                    | Fill.Color color -> 
                        ConcreteShape.fillRectangle color (Box2d.FromMinAndSize(V2d.Zero, size))
                        |> ConcreteShape.transform (m23 trafo)
                    | _ ->  
                        ()
                    match style.stroke, style.strokeWidth with
                    | Stroke.Color color, Some len when len.Value > 0.0 ->
                        ConcreteShape.rectangle color len.Value (Box2d.FromMinAndSize(V2d.Zero, size))
                        |> ConcreteShape.transform (m23 trafo)
                    | _ ->  
                        ()
                ]

            | Group(_, children) ->
                children |> List.collect (fun c ->
                    toShapeList { state with style = style } c
                )
            | Text(_, spans) ->
                let mutable index = 0
                let mutable offset = 0.0
                let mutable lastSpacing = 0.0
                spans |> List.collect (fun s ->
                    let style = style + s.props.style


                    let font =
                        match style.fontFamily with
                        | Some family -> 
                            try Font family
                            with _ -> Font "Times New Roman"
                        | None -> Font "Times New Roman"

                    let fontSize =
                        match style.fontSize with
                        | Some len -> len.ToPixels(16.0, 100.0)
                        | None -> 16.0
                    
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


                    let list = cfg.Layout s.content

                    let whiteSpaceSize = cfg.font.Spacing * fontSize

                    if index > 0 then
                        // TODO: here be dragons
                        let avg = 0.99 * lastSpacing + 0.01 * whiteSpaceSize
                        offset <- offset + avg * 0.37

                    let svgCorrection = 
                        Trafo2d.Scale(fontSize, -fontSize)

                    let toParentTrafo = 
                        Trafo2d.Translation(-list.bounds.Min.X, 0.0) * svgCorrection * Trafo2d.Translation(offset, 0.0)

                    let p = toParentTrafo.Forward.TransformPos(V2d(list.bounds.Max.X, 0.0))

                    index <- index + 1
                    lastSpacing <- whiteSpaceSize
                    offset <- p.X

                    let renderTrafo =   
                        let m4 = list.renderTrafo.Forward
                        let m = M33d.FromRows(m4.R0.XYW, m4.R1.XYW, V3d.OOI)
                        Trafo2d(m, m.Inverse)

                    let glyphTrafo =
                        renderTrafo * toParentTrafo * trafo

                    list.concreteShapes |> List.map (fun s ->
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
