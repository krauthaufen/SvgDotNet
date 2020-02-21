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
            let mat = M33d(1.0, tan x, 0.0, tan y, 1.0, 0.0, 0.0, 0.0, 1.0)
            Trafo2d(mat, mat.Inverse)
        | Matrix m ->
            let mat = M33d.op_Explicit m
            Trafo2d(mat, mat.Inverse)
        | Scale(x,y) ->
            Trafo2d.Scale(x,y)
        
    let ofStack (state : State) (l : list<Transform>) =
        l |> List.rev |> List.map (toTrafo state) |> List.fold (*) Trafo2d.Identity


module SvgProps =
    let adjustTrafo (useX : bool) (state : State) (props : SvgProps) =
        //let isGroup = false //
            //match props.style.transform with
            //| [] -> false
            //| _ -> true

        let myTrafo = Transform.ofStack state props.style.transform

        let local2Group = state.trafoInGroup //state.trafo * state.lastGroupTrafo.Inverse

        let abs =
            if useX then
                let getTrafo (dir : V2d) (x : Length) (y : Length) =
                    Trafo2d.Translation (dir * V2d(x.X state, y.Y state))
                    //let currentInGroup = local2Group.Forward.TransformPos(V2d.Zero)
                    //let targetInGroup = dir * V2d(x.X state, y.Y state)
                    //let delta = dir * (targetInGroup - currentInGroup)
                    //local2Group *
                    //Trafo2d.Translation delta *
                    //local2Group.Inverse

                match props.x, props.y with
                | Some x, Some y -> getTrafo V2d.II x y
                | Some x, None -> getTrafo V2d.IO x Length.Zero
                | None, Some y -> getTrafo V2d.OI Length.Zero y
                | None, None -> Trafo2d.Identity
            else
                Trafo2d.Identity

        let delta = 
            if useX then
                match props.dx, props.dy with
                | Some dx, Some dy -> Trafo2d.Translation(dx.X state, dy.Y state)
                | Some dx, None -> Trafo2d.Translation(dx.X state, 0.0)
                | None, Some dy -> Trafo2d.Translation(0.0, dy.Y state)
                | None, None -> Trafo2d.Identity
                
            else
                Trafo2d.Identity
        { state with
            trafo = abs * myTrafo * state.trafo
            groupTrafo = abs * myTrafo * state.groupTrafo
            trafoInGroup = Trafo2d.Identity
        }
        //if isGroup then
        //    let delta = abs * myTrafo * delta
        //    let mine = delta * state.trafo
        //    { state with
        //        trafo = mine
        //        groupTrafo = mine
        //        trafoInGroup = Trafo2d.Identity
        //    }
        //else
        //    let delta = myTrafo
        //    { state with
        //        trafo = delta * state.trafo
        //        trafoInGroup = delta * state.trafoInGroup
        //    }


module Stroke = 
    open Aardvark.Rendering.Text

    let rec intersect (epsilon : float) (a : PathSegment) (b : PathSegment) : option<float * float> =
        PathSegment.intersections epsilon a b |> List.tryHead

    let rec private tryClip (a : PathSegment) (b : seq<int * PathSegment>) : option<option<PathSegment> * int * option<PathSegment>> =
        let a1 = PathSegment.startPoint a
        let all = b |> Seq.choose (fun (i, b) -> match intersect 1E-8 a b with | Some(ta, tb) -> Some(ta, tb, i, b) | None -> None) |> Seq.toList
        match all with
        | [] ->
            None
        | _ -> 
            let (ta, tb, i, b) = all |> Seq.minBy (fun (ta,_,_,_) -> ta)

            let b0 = PathSegment.startPoint b
            if Fun.ApproximateEquals(a1, b0, 1E-8) then
                None
            else 
                Some (PathSegment.withRange 0.0 ta a, i, PathSegment.withRange tb 1.0 b)
    

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

    let private joinSegments (style : LineJoin) (miterLimit : float) (a : PathSegment) (b : PathSegment) =

        let pa = PathSegment.endPoint a
        let ta = PathSegment.tangent 1.0 a
        let ra = Ray2d(pa, ta)

        let pb = PathSegment.startPoint b
        let tb = PathSegment.tangent 0.0 b
        let rb = Ray2d(pb, -tb)

        if Fun.ApproximateEquals(pa, pb, 1E-5) then
            [], true, true
        else
            let mutable t = 0.0
            let intersects = ra.Intersects(rb, &t)
            if intersects && t > 0.0 then
                let px = ra.GetPointOnRay t
                let elements = 
                    match style with
                    | LineJoin.Miter | LineJoin.MiterClip
                    | LineJoin.Inherit | LineJoin.Unspecified | LineJoin.Arcs -> 
                        let theta = acos (clamp -1.0 1.0 (Vec.dot ta -tb))
                        let miterLength = 1.0 / sin (theta / 2.0)
                        if miterLength > miterLimit then
                            [
                                PathSegment.tryLine pa pb
                            ]
                        else
                            [
                                PathSegment.tryLine pa px
                                PathSegment.tryLine px pb
                            ]

                    | LineJoin.Bevel ->
                        [
                            PathSegment.tryLine pa pb
                        ]

                    | LineJoin.Round ->
                        let d0 = px - pa |> Vec.normalize
                        let d1 = pb - pa |> Vec.normalize
                        let angle = asin (clamp -1.0 1.0 (d0.X * d1.Y - d0.Y * d1.X))
                        if Fun.IsTiny(angle, 1.0E-8) then
                            [
                                PathSegment.tryLine pa pb
                            ]
                        else
                            [
                                PathSegment.tryArcSegment pa px pb
                            ]
                            

                elements, true, true
            elif intersects then
                let n = V2d(-ta.Y, ta.X)
                let d = Vec.dot n tb
                if d > 0.0 then
                    [], false, true
                else
                    [], false, true
            else    
                [PathSegment.tryLine pa pb], true, true


    type SegmentList(style : LineJoin, miterLimit : float, eps : float) =
        let store = System.Collections.Generic.List<PathSegment>()
        let mutable all = BvhTree2d<int, PathSegment>.Empty 6

        let setStartPoint (p : V2d) (s : PathSegment) =
            match s with
            | Line(_,p1) -> Line(p, p1)
            | Bezier2(_,p1,p2) -> Bezier2(p, p1, p2)
            | Bezier3(_,p1,p2,p3) -> Bezier3(p, p1, p2, p3)
            | Arc(_, p1, a0, a1, e) -> PathSegment.arcWithPoints p p1 a0 a1 e
            
        let setEndPoint (p : V2d) (s : PathSegment) =
            match s with
            | Line(p0,_) -> Line(p0, p)
            | Bezier2(p0,p1,_) -> Bezier2(p0, p1, p)
            | Bezier3(p0,p1,p2,_) -> Bezier3(p, p1, p2, p)
            | Arc(p0, _, a0, a1, e) -> PathSegment.arcWithPoints p0 p a0 a1 e

        let tryMerge (a : PathSegment) (b : PathSegment) =
            match a, b with
            | Line(p0, p1), Line(p2, p3) ->
                if Fun.ApproximateEquals(p1, p2, eps) then
                    let a = p1 - p0 |> Vec.normalize
                    let b = p3 - p2 |> Vec.normalize

                    let angle = acos (clamp -1.0 1.0 (Vec.dot a b))
                    if angle < 1E-4 then
                        Some (PathSegment.line p0 p3)
                    else
                        None
                else
                    None

            | Arc(_, _, a0, a1, ae), Arc(_, _, b0, b1, be) ->
                if Fun.ApproximateEquals(a1, b0) && Fun.ApproximateEquals(ae.Center, be.Center) && Fun.ApproximateEquals(ae.Axis0, be.Axis0) && Fun.ApproximateEquals(ae.Axis1, be.Axis1) then
                    Some (PathSegment.arc a0 b1 ae)
                else
                    None

            | _ ->
                // TODO: implement
                None

        member x.Add(segment : PathSegment) =
            if store.Count > 0 then 
                let li0 = store.Count - 1
                let last = store.[li0]
                match tryMerge last segment with
                | Some s -> 
                    store.RemoveAt li0
                    x.Add s
                | None ->
                    let a = PathSegment.endPoint last
                    let b = PathSegment.startPoint segment
                    if Fun.ApproximateEquals(a, b, eps) then   
                        store.Add (setStartPoint a segment)
                    else
                        let b = PathSegment.bounds segment
                        let intersecting = all.GetIntersecting(b) |> HashMap.toSeq |> Seq.map (fun (k, struct(b, v)) -> k, v)

                        match tryClip segment intersecting with
                        | Some (nl, old, ns) ->
                            all <- all.Remove(old)
                            match nl, ns with
                            | Some nl, Some ns ->
                                all <- all.Add(old, PathSegment.bounds nl, nl)
                                all <- all.Add(store.Count, PathSegment.bounds ns, ns)
                                store.[old] <- nl
                                store.Add ns
                            | None, Some n ->
                                store.RemoveAt old
                                all <- all.Remove old
                                all <- all.Add(store.Count, PathSegment.bounds n, n)
                                x.Add n
                            | Some n, None ->
                                store.[old] <- n
                                all <- all.Add(old, PathSegment.bounds n, n)
                            | None, None ->
                                store.RemoveAt old
                        | None ->       
                            let add, keepA, addSeg = joinSegments style miterLimit last segment
                            if keepA then
                                for a in add do 
                                    match a with
                                    | Some a -> x.Add a
                                    | None -> ()
                            else
                                store.RemoveAt (store.Count - 1)

                            if addSeg then x.Add segment
                                
            else
                store.Add segment

        member x.Last =
            if store.Count > 0 then Some store.[store.Count - 1]
            else None

        member x.ToArray() =
            store.ToArray()

    type SegmentSet() =
        let mutable currentId = 0
        let newId() = 
            let id = currentId
            currentId <- id + 1
            currentId

        let mutable bvh = BvhTree2d<int, PathSegment>.Empty(24)
        let mutable segments = HashMap.empty<int, PathSegment>

        member x.Add(a : PathSegment) =
            let intersecting = 
                bvh.GetIntersecting(
                    PathSegment.bounds a, 
                    fun _ _ o -> 
                        match intersect 1E-8 a o with
                        | Some (ta, tb) -> Some (ta, tb)
                        | None -> None
                )
            let intersecting =
                intersecting |> HashMap.filter (fun _ (ta, tb) -> ta > 1E-8)

            if intersecting.Count = 0 then
                let id = newId()
                segments <- HashMap.add id a segments
                bvh <- bvh.Add(id, PathSegment.bounds a, a)
            else
                let (bi, (ta, tb)) = Seq.minBy fst intersecting
                match HashMap.tryRemove bi segments with
                | Some (b, rest) ->
                    segments <- rest
                    bvh <- bvh.Remove bi

                    let (la, ra) = PathSegment.split ta a
                    let (lb, rb) = PathSegment.split tb b

                    let inline add (s : option<PathSegment>) =
                        match s with
                        | Some s ->
                            let si = newId()
                            segments <- HashMap.add si s segments
                            bvh <- bvh.Add(si, PathSegment.bounds s, s)
                        | None ->
                            ()

                    add la 

                    match ra with
                    | Some v -> x.Add v
                    | None -> ()
                    match lb with
                    | Some v -> x.Add v
                    | None -> ()
                    match rb with
                    | Some v -> x.Add v
                    | None -> ()


                | None ->
                    failwith "inconsistent"

        member x.Print() =
            let rand = RandomSystem()
            let color() = 
                let c = rand.UniformC3f().ToC4b()
                sprintf "#%02X%02X%02X" c.R c.G c.B

            printfn "<g transform=\"translate(60,60)\">"
            for (_, s) in segments do
                let c = color()
                let d = 
                    match s with
                    | Line(p0, p1) -> sprintf "M%f,%fL%f,%f" p0.X p0.Y p1.X p1.Y
                    | Bezier2(p0, p1, p2) -> sprintf "M%f,%fQ%f,%f %f,%f" p0.X p0.Y p1.X p1.Y p2.X p2.Y
                    | Bezier3(p0, p1, p2, p3) -> sprintf "M%f,%fC%f,%f %f,%f %f,%f" p0.X p0.Y p1.X p1.Y p2.X p2.Y p3.X p3.Y
                    | _ -> ""
                printfn "   <path d=\"%s\" style=\"stroke:%s\"/>" d c
            printfn "</g>"













    let rec private appendOffset (w : float) (c : SegmentSet) (p : PathSegment) =
        match p with
        | Line(p0, p1) ->
            let dir = p1 - p0
            let len = Vec.length dir
            let d = dir / len
            let n = V2d(-d.Y, d.X)
            match PathSegment.tryLine  (p0 + n * w) (p1 + n * w) with
            | Some l -> c.Add l
            | None -> ()

        | Arc(_, _, a, b, e) ->
            let r0 = e.Axis0.Length
            let r1 = e.Axis1.Length
            let d0 = e.Axis0 / r0
            let d1 = e.Axis1 / r1
            match PathSegment.tryArc a b (Ellipse2d(e.Center, d0 * (r0 + w), d1 * (r1 + w))) with
            | Some l -> c.Add l
            | None -> ()

        | Bezier2 _ ->

            let parts = 4
            let step = 1.0 / float parts
            let mutable t0 = 0.0
            let mutable t1 = step
            let mutable pLast = None
            for _ in 1 .. parts do
                match PathSegment.withRange t0 t1 p with
                | Some part ->
                    match part with
                    | Bezier2(p0, p1, p2) ->
                        let p0 = match pLast with | Some l -> l | None -> p0 + w * PathSegment.normal 0.0 part
                        let p1 = p1 + w * PathSegment.normal 0.5 part
                        let p2 = p2 + w * PathSegment.normal 1.0 part
                        pLast <- Some p2
                        match PathSegment.tryBezier2 p0 p1 p2 with
                        | Some l -> c.Add l
                        | None -> ()

                    | Line(p0, p1) ->
                        let dir = p1 - p0
                        let len = Vec.length dir
                        let d = dir / len
                        let n = V2d(-d.Y, d.X)
                        let p0 = p0 + n * w
                        let p1 = p1 + n * w
                        pLast <- Some p1
                        match PathSegment.tryLine p0 p1 with
                        | Some l -> c.Add l
                        | None -> ()

                    | _ ->
                        failwith "unreachable"
                | None ->
                    ()
                t0 <- t1
                t1 <- t1 + step


        | Bezier3(p0, p1, p2, p3)  ->
            failwith "bezier3 not handled"

    let private offsetPath (set : SegmentSet) (style : LineJoin) (miterLimit : float) (w : float) (p : Path) =
        let input = Path.toArray p
        input |> Array.iter (appendOffset w set)

        [||]





    let outsetPath (distance : float) (cap : LineCap) (join : LineJoin) (miterLimit : float) (path : Path) =
        let input = Path.toArray path
        if input.Length > 0 then
            let set = SegmentSet()

            let first = input.[0]
            let last = input.[input.Length - 1]
            let input = ()

            //let segments = System.Collections.Generic.List<PathSegment>()
            
            let add l =
                match l |> List.choose id with
                | [] -> ()
                | l -> l |> List.iter set.Add
            

            let s = -distance / 2.0

            add (
                let p = PathSegment.startPoint first
                let d = PathSegment.tangent 0.0 first
                getCap cap p d s
            )

            do
                let comp = offsetPath set join miterLimit s path
                add (comp |> Array.toList |> List.map Some)


            add (
                let p = PathSegment.endPoint last
                let d = -PathSegment.tangent 1.0 last
                getCap cap p d s
            )
                
            do
                let comp = offsetPath set join miterLimit s (Path.reverse path)
                add (comp |> Array.toList |> List.map Some)

            set.Print()

            Path.empty

        else
            Path.empty

module Pose =
    
    /// assumes coordinates (q0,q1,q2,q3) in NDC [-1,1] and a focal-length of 1.0 
    /// returns a Quad3d (P0,P1,P2,P3) that represents a 3d rectangle satisfying the following coditions:
    /// |P1-P0| = |P3-P3|
    /// |P2-P1| = |P0-P3|
    /// <P1-P0|P2-P1> = 0
    /// <P2-P1|P3-P2> = 0
    /// <P3-P2|P0-P3> = 0
    /// <P0-P3|P1-P0> = 0
    /// P0.XY/P0.Z = q0
    /// P1.XY/P1.Z = q1
    /// P2.XY/P2.Z = q2
    /// P3.XY/P3.Z = q3
    let recoverRectangle3d (image : Quad2d) =
        let v0 = image.P0
        let v1 = image.P1
        let v2 = image.P2
        let v3 = image.P3

        let inline cross (a : V2d) (b : V2d) = a.X*b.Y - a.Y*b.X

        let f = cross v0 (v2 - v1) - cross v1 v2

        if Fun.IsTiny(f) then
            None
        else
            let va = (cross v1 (v3 - v2) - cross v2 v3) / f
            let vb = (cross v0 (v3 - v2) - cross v2 v3) / f
            let vc = (cross v0 (v3 - v1) - cross v1 v3) / f

            //let f  = (v0.X * (v2.Y - v1.Y) - v0.Y * (v2.X - v1.X) - (v1.X * v2.Y - v1.Y * v2.X))
            //let va = (v1.X * (v3.Y - v2.Y) - v1.Y * (v3.X - v2.X) - (v2.X * v3.Y - v2.Y * v3.X)) / f
            //let vb = (v0.X * (v3.Y - v2.Y) - v0.Y * (v3.X - v2.X) - (v2.X * v3.Y - v2.Y * v3.X)) / f
            //let vc = (v0.X * (v3.Y - v1.Y) - v0.Y * (v3.X - v1.X) - (v1.X * v3.Y - v1.Y * v3.X)) / f 

            let rect = Quad3d(va * V3d(v0, 1.0), vb * V3d(v1, 1.0), vc * V3d(v2, 1.0), V3d(v3, 1.0))
            Some rect

    let run () =    
        let w = 4.0  
        let h = 3.0
        let rect = Quad3d(V3d(-w/2.0, -h/2.0, 0.0), V3d(w/2.0, -h/2.0, 0.0), V3d(w/2.0,h/2.0,0.0), V3d(-w/2.0,h/2.0,0.0))


        let camera = CameraView.lookAt (V3d(3.0,4.0,5.0)) (V3d(0.123, 0.1412, 0.123)) V3d.IIO.Normalized |> CameraView.viewTrafo
        let frustum = Frustum.perspective 90.0 0.01 100.0 1.0 |> Frustum.projTrafo
        let vp = camera * frustum

        let projected =
            Quad2d(
                vp.Forward.TransformPosProj(rect.P0).XY,
                vp.Forward.TransformPosProj(rect.P1).XY,
                vp.Forward.TransformPosProj(rect.P2).XY,
                vp.Forward.TransformPosProj(rect.P3).XY
            )
        printfn "projected: %s %s %s %s" (projected.P0.ToString("0.00000")) (projected.P1.ToString("0.00000")) (projected.P2.ToString("0.00000")) (projected.P3.ToString("0.00000"))

        match recoverRectangle3d projected with
        | Some recovered ->
            let e01 = recovered.P1 - recovered.P0
            let e12 = recovered.P2 - recovered.P1
            let e23 = recovered.P3 - recovered.P2
            let e30 = recovered.P0 - recovered.P3

            let aspect = Vec.length e01 / Vec.length e12


            let reproj =
                Quad2d(
                    recovered.P0.XY / recovered.P0.Z,
                    recovered.P1.XY / recovered.P1.Z,
                    recovered.P2.XY / recovered.P2.Z,
                    recovered.P3.XY / recovered.P3.Z
                )


            let inline angle a b =
                let a = Vec.normalize a
                let b = Vec.normalize b
                acos (clamp -1.0 1.0 (Vec.dot a b))


            printfn "ab: %.6f°" (Constant.DegreesPerRadian * angle e01 e12)
            printfn "bc: %.6f°" (Constant.DegreesPerRadian * angle e12 e23)
            printfn "cd: %.6f°" (Constant.DegreesPerRadian * angle e23 e30)
            printfn "da: %.6f°" (Constant.DegreesPerRadian * angle e30 e01)


            let normal =
                let ab = Vec.cross e01 e12 |> Vec.normalize
                let bc = Vec.cross e12 e23 |> Vec.normalize
                let cd = Vec.cross e23 e30 |> Vec.normalize
                let da = Vec.cross e30 e01 |> Vec.normalize
                Vec.normalize (ab + bc + cd + da)

            let plane = Plane3d(normal, recovered.ComputeCentroid())
            printfn "height(p0): %.6f" (plane.Height recovered.P0)
            printfn "height(p1): %.6f" (plane.Height recovered.P1)
            printfn "height(p2): %.6f" (plane.Height recovered.P2)
            printfn "height(p3): %.6f" (plane.Height recovered.P3)

            printfn "e01/e12: %.6f" (Vec.length e01 / Vec.length e12)
            printfn "e23/e12: %.6f" (Vec.length e23 / Vec.length e12)
            printfn "e23/e30: %.6f" (Vec.length e23 / Vec.length e30)
            printfn "e01/e30: %.6f" (Vec.length e01 / Vec.length e30)

            printfn "recovered: %s %s %s %s" (reproj.P0.ToString("0.00000")) (reproj.P1.ToString("0.00000")) (reproj.P2.ToString("0.00000")) (reproj.P3.ToString("0.00000"))
        | None ->
            printfn "failed"

        ()


[<ReflectedDefinition>]
module Shader = 
    open FShade
    type KLMKindAttribute() = inherit FShade.SemanticAttribute("KLMKind")

    type Vertex =
        {
            [<Position>] p : V4d
            [<Interpolation(InterpolationMode.Sample); KLMKind>] klmKind : V4d
            [<SamplePosition>] samplePos : V2d
            [<Color>] color : V4d
        }

    let eps = 0.00001
    [<Inline>]
    let keepsWinding (isOrtho : bool) (t : M44d) =
        if isOrtho then
            t.M00 > 0.0
        else
            let c = V3d(t.M03, t.M13, t.M23)
            let z = V3d(t.M02, t.M12, t.M22)
            Vec.dot c z < 0.0
                
    [<Inline>]
    let isOrtho (proj : M44d) = 
        abs proj.M30 < eps &&
        abs proj.M31 < eps &&
        abs proj.M32 < eps

    let pathVertex (v : Vertex) =
        vertex {
            let trafo = uniform.ModelViewTrafo

            let p = trafo * v.p
                    
            return { 
                v with 
                    p = uniform.ProjTrafo * p
                    //klm = v.klmKind.XYZ 
                    color = v.color
                }
        }
    
    type Frag =
        {
            [<Color>] color : V4d
        }

    let pathFragment(v : Vertex) =
        fragment {
            let kind = v.klmKind.W + 0.001 * v.samplePos.X
   
            let mutable color = v.color
            if kind > 1.5 && kind < 3.5 then
                // bezier2
                if uniform?Fill then
                    let ci = v.klmKind.XYZ
                    let f = (ci.X * ci.X - ci.Y) * ci.Z
                    if f > 0.0 then
                        discard()
                else
                    color <- V4d.IOOI

                        
            elif kind > 3.5 && kind < 5.5 then
                // arc
                if uniform?Fill then
                    let ci = v.klmKind.XYZ
                    let f = ((ci.X * ci.X + ci.Y*ci.Y) - 1.0) * ci.Z
                    
                    if f > 0.0 then
                        discard()
                else
                    color <- V4d.OIOI
            elif kind > 5.5  then
                if uniform?Fill then
                    let ci = v.klmKind.XYZ
                    let f = ci.X * ci.X * ci.X - ci.Y * ci.Z
                    if f > 0.0 then
                        discard()
                else
                    color <- V4d.OOII


            return { color = color }
                    
        }

let ellipseTest() =
  
    Ag.initialize()
    Aardvark.Init()
    
    
    let rand = RandomSystem()
    for i in 1 .. 10000 do rand.UniformDouble() |> ignore

    
    use app = new OpenGlApplication()
    use win = app.CreateGameWindow(8)
    let proj = win.Sizes |> AVal.map (fun s -> Trafo3d.Scale(float s.Y / float s.X, 1.0, 1.0))
    
    let scale = cval 1.0

    win.Mouse.Scroll.Values.Add (fun d ->
        transact (fun () ->
            let d1 = d / 120.0
            scale.Value <- scale.Value * (1.05 ** d1)
        )
    )


    let ts = List.init 10 (fun _ -> rand.UniformDouble())
    let e0s = 
        [
            //PathSegment.bezier2 (V2d(-0.5,0.0)) (V2d(0.0, 0.5)) (V2d(0.5, 0.0))
            //PathSegment.arc 0.0 -Constant.Pi (Ellipse2d(V2d(0.0, -0.2), 0.5*V2d.IO, 0.5*V2d.OI))
            
            let trafo = Trafo2d.Rotation(Constant.PiHalf)

            let p0 = V2d(-0.5,-0.5)  |> trafo.Forward.TransformPos
            let p1 = V2d(3.0, -0.5) |> trafo.Forward.TransformPos
            let p2 = V2d(-3.0, 0.5) |> trafo.Forward.TransformPos
            let p3 = V2d(0.5, 0.5) |> trafo.Forward.TransformPos

            yield! PathSegment.bezier3 p0 p1 p2 p3 |> PathSegment.splitMany ts


            yield! PathSegment.arc 0.0 -Constant.PiTimesTwo (Ellipse2d(V2d.Zero, 0.7 * V2d.IO, 0.5 * V2d.OI)) |> PathSegment.splitMany ts

            //PathSegment.bezier3 (V2d(-0.5,-0.2)) (V2d(-0.2, 0.7)) (V2d(0.2, 0.7)) (V2d(0.5, -0.2))
        ]

    let e1 = 
        AVal.custom (fun t ->
            let p = win.Mouse.Position.GetValue(t).Position
            let s = win.Sizes.GetValue t
            let scale = scale.GetValue t
            let ndc = 
                V3d(
                    2.0 * (float p.X / float s.X) - 1.0, 
                    1.0 - 2.0 * (float p.Y / float s.Y),
                    -1.0
                )

            let cc = proj.GetValue().Backward.TransformPosProj ndc |> Vec.xy

            let e = Ellipse2d(cc, scale * 0.2*V2d.IO, scale * 0.3*V2d.OI)
            //PathSegment.arc 0.0 -Constant.Pi e

            let trafo = Trafo2d.Scale(scale) * Trafo2d.Translation(cc)

            let p0 = V2d(-0.5,-0.5)  |> trafo.Forward.TransformPos
            let p1 = V2d(3.0, -0.5) |> trafo.Forward.TransformPos
            let p2 = V2d(-3.0, 0.5) |> trafo.Forward.TransformPos
            let p3 = V2d(0.5, 0.5) |> trafo.Forward.TransformPos


            PathSegment.arc 0.0 -Constant.PiTimesTwo e
            //PathSegment.bezier3 p0 p1 p2 p3

        )

    let intersections =
        let mutable lastPrint = -1
        e1 |> AVal.map (fun e1 ->   
            let a, b =
                e0s |> List.toArray |> Array.collect (fun e0 ->
                    let intersections = PathSegment.intersections 1E-7 e0 e1 
                    intersections
                    |> List.toArray
                    |> Array.map (fun (t0, t1) ->
                        let p0 = PathSegment.point t0 e0
                        let p1 = PathSegment.point t1 e1
                        V3f(V2f p0, -1.0f), V3f(V2f p1, -0.9f)
                    )
                )
                |> Array.unzip
            if a.Length <> lastPrint then
                lastPrint <- a.Length
                Report.Line("{0}", a.Length)
            a, b
        )

    let toGeometry (color : C4b) (s : aval<PathSegment>) =
        let positions = 
            s |> AVal.map (fun s ->
                let lines = System.Collections.Generic.List<V3f>()
                let div = 128
                let step = 1.0 / float div

                let mutable last = PathSegment.startPoint s
                let mutable t = step
                for i in 0 .. div - 1 do
                    let pt = PathSegment.point t s
                    lines.Add (V3f(V2f last, 0.0f))
                    lines.Add (V3f(V2f pt, 0.0f))
                    last <- pt
                    t <- t + step
                lines.ToArray()
            )
            
        Sg.draw IndexedGeometryMode.LineList
        |> Sg.vertexAttribute DefaultSemantic.Positions positions
        |> Sg.vertexBufferValue DefaultSemantic.Colors (AVal.constant (color.ToC4f().ToV4f()))
        
    let shapes =
        Sg.ofList [
            toGeometry C4b.Blue e1
            for e0 in e0s do
                let color = rand.UniformC3f().ToC4b()
                toGeometry color (AVal.constant e0)
        ]
        |> Sg.uniform "LineWidth" (AVal.constant 3.0)
        |> Sg.shader {
            do! DefaultSurfaces.trafo
            do! DefaultSurfaces.thickLine
            do! DefaultSurfaces.thickLineRoundCaps
        }


    let sg = 
        Sg.ofList [
            Sg.draw IndexedGeometryMode.PointList
            |> Sg.vertexAttribute DefaultSemantic.Positions (AVal.map fst intersections)
            |> Sg.vertexBufferValue DefaultSemantic.Colors (AVal.constant V4f.IOOI)
            |> Sg.uniform "PointSize" (AVal.constant 5.0)
        
            Sg.draw IndexedGeometryMode.PointList
            |> Sg.vertexAttribute DefaultSemantic.Positions (AVal.map snd intersections)
            |> Sg.vertexBufferValue DefaultSemantic.Colors (AVal.constant V4f.OIOI)
            |> Sg.uniform "PointSize" (AVal.constant 7.0)
        ]
        |> Sg.andAlso shapes
        |> Sg.projTrafo proj
        |> Sg.shader {
            do! DefaultSurfaces.trafo
            do! DefaultSurfaces.pointSprite
            do! DefaultSurfaces.pointSpriteFragment
        }

    win.RenderTask <- app.Runtime.CompileRender(win.FramebufferSignature, sg)
    win.Run()


[<EntryPoint;STAThread>]
let main argv = 
    //ellipseTest()
    //Environment.Exit 0

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
                let state = getState true state n.props
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
                let state = getState true state n.props
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
                let state = getState true state n.props
                match style.stroke, style.strokeWidth with
                | Stroke.Color color, Some w when w.Value > 0.0 ->
                    let p0 = p0.ToV2d state
                    let p1 = p1.ToV2d state

                    let c = Vec.normalize (p1 - p0)
                    let n = V2d(-c.Y, c.X)
                    let pc = 0.5 * (p0 + p1) + n * 20.0

                    let path = Path.ofList [ PathSegment.line p0 p1 ]
                    let miterLimit = defaultArg style.miterLimit 4.0
                    let path = Stroke.outsetPath w.Value style.lineCap style.lineJoin miterLimit path

                    [ ConcreteShape.ofPath state.trafo.Forward color path ]

                | _ ->
                    []

            | Polyline pts ->
                let state = getState true state n.props
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
                let state = getState true state n.props
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

                    let props =
                        { s.props with
                            x = match s.props.x with | Some x -> Some x | None -> n.props.x
                            y = match s.props.y with | Some y -> Some y | None -> n.props.y
                            dx = match s.props.dx with | Some dx -> Some dx | None -> n.props.dx
                            dy = match s.props.dy with | Some dy -> Some dy | None -> n.props.dy
                        }

                    state <- getState true { state with style = style } props

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

                    //let shapes =
                    //    shapes |> List.map (fun s ->
                    //        //let path = s.shape.Path |> Stroke.outsetPath 1.0 LineCap.Butt LineJoin.Miter 4.0 
                    //        { s with shape = Shape path }
                    //    )

                    shapes |> List.map (fun s ->
                        s |> ConcreteShape.transform (m23 glyphTrafo)
                    )

                )
            | Path instructions ->  
                let state = getState true state n.props

                let result = System.Collections.Generic.List<PathSegment>()
                let mutable start = V2d.Zero
                let mutable current = V2d.Zero
                for i in instructions do
                    match i with
                    | Move p ->
                        current <- p
                        start <- p
                    | LineTo p ->
                        match PathSegment.tryLine current p with
                        | Some l -> result.Add l
                        | None -> ()
                        current <- p
                    | QuadraticTo(c, p) ->  
                        match PathSegment.tryBezier2 current c p with
                        | Some l -> result.Add l
                        | None -> ()
                        current <- p
                    | CurveTo(c0, c1, pg) ->
                        match PathSegment.tryBezier3 current c0 c1 pg with
                        | Some l ->     
                            //match PathSegment.inflectionParameters l with
                            //| [t] when t >= 1E-8 && t <= 1.0 - 1E-8 ->
                            //    let a, b = PathSegment.split t l
                            //    match a with | Some l -> result.Add l | None -> ()
                            //    match b with | Some r -> result.Add r | None -> ()
                            //| ts -> 
                                result.Add l
                        | None -> 
                            ()
                        current <- pg
                    | ClosePath ->
                        if not (Fun.ApproximateEquals(current, start)) then
                            match PathSegment.tryLine current start with
                            | Some l -> result.Add l
                            | None -> ()
                        current <- start
                    | Arc(p1, rx, ry, rotation, large, sweep) ->
                        let p0 = current

                        let m = M33d.Translation(p0) * M33d.Rotation(-rotation * Constant.RadiansPerDegree) * M33d.Scale(rx, ry)
                        let mi = m.Inverse

                        let q0 = mi.TransformPos p0
                        let q1 = mi.TransformPos p1

                        let d = q1 - q0 
                        let l = Vec.length d
                        let mp = 0.5 * (q0 + q1)
                        let e0, e1 = 
                            if l >= 2.0 then
                                let c = mp
                                let r = l / 2.0
                                let e = Ellipse2d.FromConjugateDiameters(m.TransformPos c, m.TransformDir(V2d(r, 0.0)), m.TransformDir(V2d(0.0, r)))
                                e, e
                            else
                                let r = l / 2.0
                                let n = V2d(-d.Y, d.X) / l
                                let d = sqrt (1.0 - sqr r)

                                let c0 = mp + n * d
                                let c1 = mp - n * d
                                let e0 = Ellipse2d.FromConjugateDiameters(m.TransformPos c0, m.TransformDir(V2d(1.0, 0.0)), m.TransformDir(V2d(0.0, 1.0)))
                                let e1 = Ellipse2d.FromConjugateDiameters(m.TransformPos c1, m.TransformDir(V2d(1.0, 0.0)), m.TransformDir(V2d(0.0, 1.0)))
                                e0, e1


                        let ellipse = e0

                        let a0 = ellipse.GetAlpha p0
                        let a1 = ellipse.GetAlpha p1

                        if large then
                            let da = a1 - a0
                            let angle = 
                                if abs da < Constant.Pi then 
                                    -float (sign da) * (Constant.PiTimesTwo - abs da)
                                else 
                                    da

                            let parts = abs angle / Constant.PiHalf |> ceil |> int
                            let step = angle / float parts
                            let mutable a = a0

                            let tangent (alpha : float) =
                                let pos = ellipse.Center + cos alpha * ellipse.Axis0 + sin alpha * ellipse.Axis1
                                let dir = cos alpha * ellipse.Axis1 - sin alpha * ellipse.Axis0 |> Vec.normalize
                                Ray2d(pos, dir)

                            
                            let mutable t0 = tangent a

                            for i in 1 .. parts do
                                let n = 
                                    if i = parts then a1
                                    else a + step

                                let t1 = tangent n
                                let c = t0.Intersect(t1)

                                match PathSegment.tryArcSegment t0.Origin c t1.Origin with
                                | Some a -> result.Add a
                                | None -> ()

                                a <- n
                                t0 <- t1

                            printfn "%A %A" angle parts
                        else
                            match PathSegment.tryArc a0 a1 ellipse with
                            | Some a -> result.Add a
                            | None -> ()

                        current <- ellipse.GetPoint a1








      
                let path = Path.ofSeq (result |> Seq.map PathSegment.reverse |> Seq.rev)
                let style = state.style

                [
                    match style.fill with
                    | Fill.Color c -> 
                        ConcreteShape.ofPath state.trafo.Forward c path
                    | _ ->
                        ()
                        
                    match style.stroke, style.strokeWidth with
                    | Stroke.Color c, Some w when w.Value > 0.0 -> 
                        let miterLimit = defaultArg style.miterLimit 4.0
                        let pp = Stroke.outsetPath w.Value style.lineCap style.lineJoin miterLimit path
                        ConcreteShape.ofPath state.trafo.Forward c pp
                    | _ ->
                        ()


                ]
    
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



    //let path = @"C:\Users\Schorsch\Development\SvgDotNet\data\Example.svg" //Path.combine [__SOURCE_DIRECTORY__; ".."; ".."; "data"; "Example.svg"] //@"C:\Users\Schorsch\Downloads\Example_svg.svg"

    //let shapes = readShape path |> cval



    Ag.initialize()
    Aardvark.Init()

    use app = new OpenGlApplication()
    use win = app.CreateGameWindow(8)


    let initialView = CameraView.lookAt (V3d(0,6,0)) V3d.Zero V3d.OOI
    let view = initialView |> DefaultCameraController.control win.Mouse win.Keyboard win.Time
    let proj = win.Sizes |> AVal.map (fun s -> Frustum.perspective 60.0 0.1 100.0 (float s.X / float s.Y))

    //let trafo = 
    //    shapes |> AVal.map (fun shapes ->
    //        let bb = shapes.bounds
    //        let off = V3d(-bb.Center.XY, 0.0)
    //        let scale = 4.0 / bb.Size.NormMax
    //        Trafo3d.Translation off *
    //        Trafo3d.Scale(scale, -scale, 1.0)
    //    )

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

        //| Keys.Enter | Keys.R | Keys.F5 ->
        //    transact (fun () ->
        //        shapes.Value <- readShape path
        //    )

        | _ ->
            ()
    )

    
    let hull, real =
        Tessellator.Tessellator.toGeometry LibTessDotNet.Double.WindingRule.NonZero [

            PathSegment.arc 0.0 Constant.PiTimesTwo (Ellipse2d(V2d.Zero, 0.5*V2d.IO, 0.5*V2d.OI))
            //PathSegment.arc 0.0 Constant.PiTimesTwo (Ellipse2d(V2d.Zero, V2d.IO, 0.2*V2d.OI))

            //PathSegment.arcSegment (V2d(-1.0, -0.1)) (V2d(0.0, -1.0)) (V2d(1.0, -0.1))

            
            PathSegment.bezier3 (V2d(-1.0, -0.1)) (V2d(-0.4,1.7)) (V2d(0.4, -0.4)) (V2d(1.0, -0.1))

            PathSegment.line (V2d(1.0, -0.1)) (V2d(1.0, 0.1))
            PathSegment.bezier2  (V2d(1.0, 0.1)) (V2d(0.0, 0.8)) (V2d(-1.0, 0.1))
            PathSegment.line (V2d(-1.0, 0.1)) (V2d(-1.0, -0.1))
            
            //PathSegment.bezier2 (V2d(-0.7, 0.0)) (V2d(0.0, 0.7)) (V2d(0.7, 0.0))
            //PathSegment.arcSegment (V2d(0.7, 0.0)) (V2d(0.0, -0.7)) (V2d(-0.7, 0.0))

            //PathSegment.arc  0.0 (4.0 * Constant.PiHalf) (Ellipse2d(V2d.Zero, 0.1*V2d.IO, 0.05*V2d.OI))
            //PathSegment.line V2d.OI V2d.OO
            //PathSegment.line V2d.OO V2d.IO

        ]


   
    let sg =
        Sg.ofList [
            
            Sg.ofList [
                Sg.ofIndexedGeometry real
            ]
            |> Sg.shader {
                do! Shader.pathVertex
                do! DefaultSurfaces.constantColor C4f.Black
                do! Shader.pathFragment
            }
            //Sg.ofList [
            //    Sg.ofIndexedGeometry hull
            //]
            //|> Sg.shader {
            //    do! Shader.pathVertex
            //    do! DefaultSurfaces.constantColor (C4f(1.0f, 1.0f, 1.0f, 0.5f))
            //    do! Shader.pathFragment
            //}
            //|> Sg.blendMode (AVal.constant BlendMode.Blend)
            //|> Sg.depthTest (AVal.constant DepthTestMode.None)
            //|> Sg.pass (RenderPass.after "" RenderPassOrder.Arbitrary RenderPass.main)
        ]
        |> Sg.uniform "LineWidth" (AVal.constant 5.0)
        //Sg.shape shapes
        //|> Sg.trafo trafo
        |> Sg.fillMode fill
        |> Sg.uniform "Fill" (fill |> AVal.map ((=) FillMode.Fill))
        |> Sg.transform (Trafo3d.RotationX(Constant.PiHalf))
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
