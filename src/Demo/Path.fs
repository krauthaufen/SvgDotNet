namespace Tessellator

open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Rendering.Text

module Tessellator =
    open LibTessDotNet.Double
    [<AutoOpen>]
    module private Helpers = 

    
        type Ellipse2d with
            member internal x.GetControlPoint(alpha0 : float, alpha1 : float) =
                let p0 = x.GetPoint alpha0
                let p1 = x.GetPoint alpha1
                let t0 = cos alpha0 * x.Axis1 - sin alpha0 * x.Axis0 |> Vec.normalize
                let t1 = cos alpha1 * x.Axis1 - sin alpha1 * x.Axis0 |> Vec.normalize
                let n0 = Ray2d(p0, t0)
                let n1 = Ray2d(p1, t1)
                let pc = n0.Intersect(n1)
                pc

        type Part =
            | PolygonalPart of PathSegment * Polygon2d
            | LinePart of Line2d

        module Part =
            let overlaps (l : Part) (r : Part) =
                match l, r with
                | LinePart l, LinePart r -> l.Intersects r
                | PolygonalPart(_,l), LinePart r -> l.Intersects(r)
                | LinePart l, PolygonalPart(_,r) -> r.Intersects(l)
                | PolygonalPart(_,l), PolygonalPart(_,r) -> l.Intersects r

            let ofPathSegment (s : PathSegment) =
                match s with
                | Line(p0, p1) -> 
                    LinePart(Line2d(p0, p1))
                | Bezier2(p0, p1, p2) ->
                    PolygonalPart(s, Polygon2d [| p0; p1; p2 |])
                | Bezier3(p0, p1, p2, p3) ->
                    PolygonalPart(s, Polygon2d [| p0; p1; p2; p3 |])
                | Arc(p0, p2, alpha0, dAlpha, ellipse) ->
                    let p1 = ellipse.GetControlPoint(alpha0, alpha0 + dAlpha)
                    PolygonalPart(s, Polygon2d [| p0; p1; p2 |])

            let bounds (p : Part) =
                match p with
                | LinePart l -> l.BoundingBox2d
                | PolygonalPart(_,p) -> p.BoundingBox2d
            
            let toPathSegment (s : Part) =
                match s with
                | LinePart l -> PathSegment.line l.P0 l.P1
                | PolygonalPart(s,_) -> s

            let toLineArray (p : Part) =
                match p with
                | LinePart(l) -> 
                    [| l.P0; l.P1 |]
                | PolygonalPart(_,p) ->
                    let arr = Array.zeroCreate (2 * p.PointCount)
                    let mutable oi = 0
                    let mutable last = p.[p.PointCount - 1]
                    for i in 0 .. p.PointCount - 1 do
                        let p = p.[i]
                        arr.[oi] <- last
                        arr.[oi+1] <- p
                        last <- p
                        oi <- oi + 2
                    arr

            let startPoint (p : Part) =
                match p with
                | PolygonalPart(s,_) ->
                    match s with
                    | Line(p0, p1) -> p0
                    | Bezier2(p0, p1, _) -> p0
                    | Bezier3(p0, p1, _, _) -> p0
                    | Arc(p0, _, _, _, _) -> p0
                | LinePart l -> 
                    l.P0
            
            let endPoint (p : Part) =
                match p with
                | PolygonalPart(s,_) ->
                    match s with
                    | Line(_, p1) -> p1
                    | Bezier2(_, _, p2) -> p2
                    | Bezier3(_, _, _, p3) -> p3
                    | Arc(_, p1, _, _, _) -> p1
                | LinePart l -> 
                    l.P1

            let startTangent (p : Part) =
                match p with
                | PolygonalPart(s,_) ->
                    PathSegment.tangent 0.0 s
                   
                | LinePart l -> 
                    Vec.normalize (l.P1 - l.P0)
            
            let endTangent (p : Part) =
                match p with
                | PolygonalPart(s,_) ->
                    PathSegment.tangent 1.0 s
                | LinePart l -> 
                    Vec.normalize (l.P1 - l.P0)

        let toBvh (positions : V2d[], triangleIndex : int[]) =
            let mutable bvh = BvhTree2d.empty
        
            let mutable bi = 0
            for ti in 0 .. triangleIndex.Length / 3 - 1 do
                let i0 = triangleIndex.[bi]
                let i1 = triangleIndex.[bi+1]
                let i2 = triangleIndex.[bi+2]
                let tri = Triangle2d(positions.[i0], positions.[i1], positions.[i2])

                bvh <- bvh.Add(ti, tri.BoundingBox2d, tri)

                bi <- bi + 3
            bvh

        let containsPoint (bvh : BvhTree2d<int, Triangle2d>) (pt : V2d) =
            let b = Box2d.FromCenterAndSize(pt, V2d.II * 1E-8)

            let contains (t : Triangle2d) (pt : V2d) =
                
                let u = t.P1 - t.P0
                let v = t.P2 - t.P1

                let ccw = u.X*v.Y - u.Y*v.X > 0.0
                let intersects = 
                    if ccw then t.Contains pt
                    else Triangle2d(t.P0, t.P2, t.P1).Contains pt

                if intersects then Some t
                else None
                //let m = M33d.FromCols(V3d(u, 0.0), V3d(v, 0.0), V3d(t.P0, 1.0))
                //let mi = m.Inverse
                //let uv = mi.TransformPos pt

                //if uv.X >= -1E-9 && uv.Y >= -1E-9 && uv.X + uv.Y <= 1.0 + 1E-9 then
                //    if not (t.Contains pt) then Log.warn "asdsadlnmaskmdkasmdkl"
                //    Some t
                //else
                //    None




            bvh.GetIntersecting(b, fun _ _ t -> contains t pt)
            |> HashMap.isEmpty
            |> not

        let hasPoint (bvh : BvhTree2d<int, Triangle2d>) (pt : V2d) =
            let b = Box2d.FromCenterAndSize(pt, V2d.II * 1E-8)
            bvh.GetIntersecting(b, fun _ _ t -> if Fun.ApproximateEquals(t.P0, pt) || Fun.ApproximateEquals(t.P1, pt) || Fun.ApproximateEquals(t.P2, pt) then Some t else None)
            |> HashMap.isEmpty
            |> not

        let boundary (rule : WindingRule) (parts : seq<#seq<V2d>>) =
            let tess = Tess()

            for path in parts do
                let vertices = System.Collections.Generic.List<ContourVertex>()

                for pt in path do
                    ContourVertex(Vec3(X = pt.X, Y = pt.Y)) |> vertices.Add

                tess.AddContour(vertices)

            tess.Tessellate(rule, ElementType.BoundaryContours)
        
            let mutable pi = 0
            let mutable i = 0
            let res = Array.zeroCreate tess.ElementCount
            for _ in 0 .. tess.ElementCount - 1 do
                let i0 = tess.Elements.[i]
                let cnt = tess.Elements.[i+1]
                let arr = Array.zeroCreate cnt

                let mutable oi = 0
                for vi in i0 .. i0 + cnt - 1 do
                    let v = tess.Vertices.[vi]
                    arr.[oi] <- V2d(v.Position.X, v.Position.Y)
                    oi <- oi + 1

                res.[pi] <- arr
                pi <- pi + 1
                i <- i + 2
            res
        
        let triangulate (rule : WindingRule) (parts : seq<#seq<V2d>>) =
            let tess = Tess()

            for path in parts do
                let vertices = System.Collections.Generic.List<ContourVertex>()

                for pt in path do
                    ContourVertex(Vec3(X = pt.X, Y = pt.Y)) |> vertices.Add

                vertices.Add vertices.[0]
                tess.AddContour(vertices)

            tess.Tessellate(rule, ElementType.Polygons, 3)
            let positions = tess.Vertices |> Array.map (fun v -> V2d(v.Position.X, v.Position.Y))
            let index = tess.Elements
            positions, index


    
    module private Coords =
        let bezier3 (p0 : V2d) (p1 : V2d) (p2 : V2d) (p3 : V2d) =
            let m3 =
                M44d(
                    +1.0,  0.0,  0.0,  0.0,
                    -3.0,  3.0,  0.0,  0.0,
                    +3.0, -6.0,  3.0,  0.0,
                    -1.0,  3.0, -3.0,  1.0
                )
                
            let m3i =
                M44d(
                    1.0,  0.0,      0.0,     0.0,
                    1.0,  1.0/3.0,  0.0,     0.0,
                    1.0,  2.0/3.0,  1.0/3.0, 0.0,
                    1.0,  1.0,      1.0,     1.0
                ) 

            let b =
                M44d.FromRows(
                    V4d(p0.X, p0.Y, 0.0, 1.0),
                    V4d(p1.X, p1.Y, 0.0, 1.0),
                    V4d(p2.X, p2.Y, 0.0, 1.0),
                    V4d(p3.X, p3.Y, 0.0, 1.0)
                )

            let bla = m3 * b
            let p0 = bla.R0.XYW
            let p1 = bla.R1.XYW
            let p2 = bla.R2.XYW
            let p3 = bla.R3.XYW

            let d0 =  M33d.FromRows(p3, p2, p1).Det
            let d1 = -M33d.FromRows(p3, p2, p0).Det
            let d2 =  M33d.FromRows(p3, p1, p0).Det
            let d3 = -M33d.FromRows(p2, p1, p0).Det

            let delta1 = d0*d2 - d1*d1
            let delta2 = d1*d2 - d0*d3
            let delta3 = d1*d3 - d2*d2
            let discr = 4.0*delta1*delta3 - delta2*delta2
        
            let inline flip (v : V3d) = V3d(-v.X, -v.Y, v.Z)

            let qs2 = 3.0*d2*d2 - 4.0*d1*d3

            if qs2 >= 0.0 && not (Fun.IsTiny(d1, 1E-8)) then   
                // serpentine / Cusp with inflection at infinity
                let qs = sqrt(qs2 / 3.0)
                let l = V2d(d2 + qs, 2.0*d1).Normalized
                let m = V2d(d2 - qs, 2.0*d1).Normalized

                let tl = l.X
                let sl = l.Y
                let tm = m.X
                let sm = m.Y

                let F =
                    M44d(
                        tl*tm,          tl*tl*tl,                   tm*tm*tm,                   1.0,
                        -sm*tl-sl*tm,   -3.0*sl*tl*tl,              -3.0*sm*tm*tm,              0.0,
                        sl*sm,          3.0*sl*sl*tl,               3.0*sm*sm*tm,               0.0,
                        0.0,            -sl*sl*sl,                  -sm*sm*sm,                  0.0
                    )

                let shouldFlip = d1 > 0.0 // TODO
                let w = m3i * F
                if shouldFlip then Choice1Of2(7, flip w.R0.XYZ, flip w.R1.XYZ, flip w.R2.XYZ, flip w.R3.XYZ)
                else Choice1Of2(7, w.R0.XYZ, w.R1.XYZ, w.R2.XYZ, w.R3.XYZ)

            elif qs2 < 0.0 && not (Fun.IsTiny(d1, 1E-8)) then 
                // loop
                let ql = sqrt(-qs2)
                let d = V2d(d2 + ql, 2.0*d1).Normalized
                let e = V2d(d2 - ql, 2.0*d1).Normalized

                let td = d.X
                let sd = d.Y
                let te = e.X
                let se = e.Y

                let a = td / sd
                let b = te / se

                let va = a >= 1E-3 && a <= 1.0 - 1E-3
                let vb = b >= 1E-3 && b <= 1.0 - 1E-3

                if va then 
                    Log.warn "split %A" a
                    Choice2Of2 a
                elif vb then 
                    Log.warn "split %A" b
                    Choice2Of2 b
                else
                    let F =
                        M44d(
                            td*te,          td*td*te,                   td*te*te,                   1.0,
                            -se*td-sd*te,   -se*td*td-2.0*sd*te*td,     -sd*te*te-2.0*se*td*te,     0.0,
                            sd*se,          te*sd*sd+2.0*se*td*sd,      td*se*se+2.0*sd*te*se,      0.0,
                            0.0,            -sd*sd*se,                  -sd*se*se,                  0.0
                        )

                    let w = m3i * F

                    let shouldNotFlip = (d1 > 0.0 && w.M10 < 0.0) || (d1 < 0.0 && w.M10 > 0.0)

                    if not shouldNotFlip then Choice1Of2(8, flip w.R0.XYZ, flip w.R1.XYZ, flip w.R2.XYZ, flip w.R3.XYZ)
                    else Choice1Of2(8, w.R0.XYZ, w.R1.XYZ, w.R2.XYZ, w.R3.XYZ)

            elif Fun.IsTiny(d1, 1E-8) && not (Fun.IsTiny(d1, 1E-8)) then
                // Cusp with cusp at infinity


                let l = V2d(d3, 3.0*d2).Normalized
                let tl = l.X
                let sl = l.Y

                let F =
                    M44d(
                        tl,         tl*tl*tl,       1.0, 1.0,
                        -sl,        -3.0*sl*tl*tl,  0.0, 0.0,
                        0.0,        3.0*sl*sl*tl,   0.0, 0.0,
                        0.0,        -sl*sl*sl,      0.0, 0.0
                    ) 
            
                let w = m3i * F
                Choice1Of2(6, w.R0.XYZ, w.R1.XYZ, w.R2.XYZ, w.R3.XYZ)
        
            elif Fun.IsTiny(d1, 1E-8) && Fun.IsTiny(d2, 1E-8) && not (Fun.IsTiny(d3, 1E-8)) then
                // quadratic
                failwith "quadratic"

            else
                failwith "line or point"


    /// assumptions:
    /// 1. the path is not self-intersecting
    let toGeometry (rule : WindingRule) (path : seq<PathSegment>) =

        let split (part : Part) =
            match part with
            | PolygonalPart(s, _) ->
                let (l,r) = PathSegment.split 0.5 s
                match l, r with
                | Some l, Some r -> 
                    [l;r]
                | Some v, None 
                | None, Some v ->
                    [v]
                | None, None ->
                    []
            | LinePart l ->
                [PathSegment.line l.P0 l.P1]

        let turningAngle (t0 : V2d) (t1 : V2d) =
            let a = t0.X * t1.Y - t0.Y * t1.X
            let b = t0.X * t1.X + t0.Y * t1.Y
            atan2 a b
            
        let neededSplits (s : PathSegment) =
            match s with
            | Bezier3(p0, p1, p2, p3) ->
                match Coords.bezier3 p0 p1 p2 p3 with
                | Choice2Of2 t ->
                    let (l, r) = PathSegment.split t s
                    match l with
                    | Some l ->
                        match r with
                        | Some r -> Some [l;r]
                        | None -> Some [l]
                    | None ->
                        match r with
                        | Some r -> Some [r]
                        | None -> Some []
                | _ ->
                    None
            | Arc(p0, p1, alpha0, dAlpha, ellipse) ->
                let steps = dAlpha / Constant.PiQuarter |> abs |> round |> int |> max 1
                if steps > 1 then
                    let mutable a0 = alpha0
                    let mutable p0 = p0
                    let step = dAlpha / float steps
                    let res =
                        List.init steps (fun i ->
                            let a1 = a0 + step
                            let p1 = if i = steps - 1 then p1 else ellipse.GetPoint a1

                            let seg = PathSegment.arcWithPoints p0 p1 a0 step ellipse

                            a0 <- a1
                            p0 <- p1
                            seg
                        )
                    Some res
                else
                    None
            | _ ->
                None
                    

        let rec nonOverlapping (result : IndexList<Part>) (bvh : BvhTree2d<Index, Part>) (stack : list<PathSegment>) =
            match stack with
            | s :: stack ->
                let replacements = neededSplits s
                    
                match replacements with
                | Some repl ->
                    nonOverlapping result bvh (repl @ stack)
                | None -> 
                    let part = Part.ofPathSegment s
                    let b = Part.bounds part
                
                    let intersecting =
                        bvh.GetIntersecting(b, fun _ _ other -> 
                            if Part.overlaps part other then Some other
                            else None
                        )

                    if HashMap.isEmpty intersecting then
                        let idx = Index.after result.MaxIndex
                        let result1 = IndexList.set idx part result
                        let bvh1 = bvh.Add(idx, b, part)
                        nonOverlapping result1 bvh1 stack
                    else    
                        let (bvh, result) =
                            intersecting 
                            |> HashMap.choose (fun _ o -> match o with | PolygonalPart _ -> Some o | _ -> None)
                            |> HashMap.fold (fun (obvh : BvhTree2d<Index, Part>, result : IndexList<Part>) idx part ->
                                let bvh = BvhTree2d.remove idx obvh
                                match split part with
                                | [] ->     
                                    (bvh, IndexList.remove idx result)
                                | [n] ->
                                    let np = Part.ofPathSegment n
                                    (BvhTree2d.add idx (Part.bounds np) np bvh, IndexList.set idx np result)
                                | n :: many ->
                                    let np = Part.ofPathSegment n
                                    let mutable bvh = BvhTree2d.add idx (Part.bounds np) np bvh
                                    let mutable result = IndexList.set idx np result
                                    let mutable idx = idx
                                    for m in many do
                                        let i =
                                            let (_,_,r) = IndexList.neighbours idx result
                                            match r with
                                            | Some(ri, _) -> Index.between idx ri
                                            | None -> Index.after idx

                                        let mp = Part.ofPathSegment m
                                        result <- IndexList.set i mp result
                                        bvh <- BvhTree2d.add i (Part.bounds mp) mp bvh
                                        idx <- i

                                    (bvh, result)
                            ) (bvh, result)

                        let self = split part
                        nonOverlapping result bvh (self @ stack)
            | _ ->
                result |> IndexList.toList

        let cleaned = nonOverlapping IndexList.empty BvhTree2d.empty (Seq.toList path)

        let rec findClosed (exteriorAngle : float) (last : V2d) (current : IndexList<Part>) (stack : list<Part>) =
            match stack with
            | part :: stack ->
                let p0 = Part.startPoint part
                let p1 = Part.endPoint part

                if Fun.ApproximateEquals(last, p0) || last.IsNaN then
                    let t0 = Part.startTangent part
                    let t1 = Part.endTangent part
                    let selfAngle = turningAngle t0 t1
                    let cornerAngle = 
                        match IndexList.tryLast current with
                        | Some last -> 
                            let tl = Part.endTangent last
                            turningAngle tl t0
                        | _ -> 
                            0.0
                    let idx = Index.after current.MaxIndex
                    let result1 = IndexList.set idx part current
                    let exteriorAngle1 = exteriorAngle + selfAngle + cornerAngle

                    match Seq.tryHead result1 with
                    | Some start when Fun.ApproximateEquals(Part.startPoint start, p1, 1E-8) ->
                        let cornerAngle = turningAngle (Part.endTangent part) (Part.startTangent start)
                        let exteriorAngle2 = exteriorAngle1 + cornerAngle
                            
                        let rest = findClosed 0.0 V2d.NaN IndexList.empty stack

                        (exteriorAngle2, result1) :: rest
                    | _ -> 
                        findClosed exteriorAngle1 p1 result1 stack
                else
                    match Seq.tryHead current with
                    | Some start ->
                        findClosed exteriorAngle last current (LinePart(Line2d(last, Part.startPoint start)) :: part :: stack)
                    | _ ->   
                        findClosed 0.0 V2d.NaN IndexList.empty stack
            | [] ->
                match Seq.tryHead current with
                | Some start ->
                    findClosed exteriorAngle last current [LinePart(Line2d(last, Part.startPoint start))]
                | _ ->      
                    []
        
        let parts = findClosed 0.0 V2d.NaN IndexList.empty cleaned |> IndexList.ofList

        let nonCurvedTriangles =
            parts |> Seq.choose (fun (_,path) ->
                if path.Count > 0 then
                    let first = Seq.head path
                    let rest = path |> Seq.map (fun p -> Part.endPoint p)
                    Seq.append (Seq.singleton (Part.startPoint first)) rest |> Some
                else
                    None
            )
            |> boundary rule
            |> triangulate WindingRule.EvenOdd

        let nonCurved =
            nonCurvedTriangles
            |> toBvh

        let tess = Tess()
        let polygons = System.Collections.Generic.List<PathSegment * Polygon2d>()
        for (exterior, path) in parts do
            let contour = System.Collections.Generic.List<ContourVertex>()

            let inline add (v : V2d) =
                contour.Add(ContourVertex(Vec3(X = v.X, Y = v.Y)))

            for part in path do
                if hasPoint nonCurved (Part.startPoint part) && hasPoint nonCurved (Part.endPoint part) then
                    match part with
                    | LinePart l ->
                        add l.P0
                        add l.P1
                    | PolygonalPart(s, p) ->
                        polygons.Add(s, p)
                        match s with
                        | Line(p0, p1) ->
                            add p0
                            add p1
                        | Bezier2(p0, p1, p2) ->
                            add p0
                            if containsPoint nonCurved p1 then add p1
                            add p2
                        | Bezier3(p0, p1, p2, p3) ->
                            add p0
                            if containsPoint nonCurved p1 then add p1
                            if containsPoint nonCurved p2 then add p2
                            add p3
                        | Arc(p0, p2, alpha0, dAlpha, ellipse) ->
                            let p1 = ellipse.GetControlPoint(alpha0, alpha0 + dAlpha)
                            add p0 
                            if containsPoint nonCurved p1 then add p1
                            add p2
                        
            if contour.Count > 0 then
                tess.AddContour(contour)

             
        tess.Tessellate(WindingRule.EvenOdd, ElementType.Polygons, 3)

        let positions = 
            tess.Elements |> Array.map (fun i ->
                let v = tess.Vertices.[i]
                V3f(v.Position.X, v.Position.Y, 0.0)
            ) |> System.Collections.Generic.List

        let coords = 
            Array.init positions.Count (fun _ -> V4f.Zero) |> System.Collections.Generic.List

        for (seg, _) in polygons do 
            let inline fw (kind : int) (c : V3d) = V4f(float32 c.X, float32 c.Y, float32 c.Z, float32 kind)
            let inline bw (kind : int) (c : V3d) = V4f(float32 -c.X, float32 -c.Y, float32 c.Z, float32 kind)
            match seg with
            | Line _ -> ()
            | Bezier2(p0, p1, p2) ->
                positions.AddRange [| V3f(V2f p0, 0.0f); V3f(V2f p1, 0.0f); V3f(V2f p2, 0.0f) |]

                if containsPoint nonCurved p1 then 
                    coords.AddRange [| V4f(0,0,-1,2); V4f(-0.5, 0.0,-1.0, 2.0); V4f(-1,1,-1,2)|]
                else
                    coords.AddRange [|V4f(0,0,1,3); V4f(0.5, 0.0, 1.0,3.0); V4f(1,1,1,3)|]


            | Arc(p0, p2, alpha0, dAlpha, ellipse) ->   
                let uv2World = M33d.FromCols(V3d(ellipse.Axis0, 0.0), V3d(ellipse.Axis1, 0.0), V3d(ellipse.Center, 1.0))
                let world2UV = uv2World.Inverse
                let p1 = ellipse.GetControlPoint(alpha0, alpha0 + dAlpha)

                let c0 = world2UV.TransformPos p0
                let c1 = world2UV.TransformPos p1
                let c2 = world2UV.TransformPos p2

                positions.AddRange [  V3f(V2f p0, 0.0f); V3f(V2f p1, 0.0f); V3f(V2f p2, 0.0f) ]
                if containsPoint nonCurved p1 then
                    coords.AddRange [V4f(c0.X, c0.Y,-1.0, 4.0); V4f(c1.X, c1.Y,-1.0, 4.0); V4f(c2.X, c2.Y,-1.0,4.0)]
                else
                    coords.AddRange [V4f(c0.X, c0.Y,1.0, 5.0); V4f(c1.X, c1.Y,1.0, 5.0); V4f(c2.X, c2.Y,1.0,5.0)]
                    

            | _ ->
                failwith ""




        let solid = 
            IndexedGeometry(
                Mode = IndexedGeometryMode.TriangleList,
                IndexArray = (snd nonCurvedTriangles), 
                IndexedAttributes =
                    SymDict.ofList [
                        DefaultSemantic.Positions, (nonCurvedTriangles |> fst |> Array.map (fun v -> V3f(V2f v, 0.0f)) :> System.Array) //|> positions.ToArray() :> System.Array
                        Symbol.Create "KLMKind", (nonCurvedTriangles |> fst |> Array.map (fun v -> V4f.Zero) :> System.Array) //coords.ToArray() :> System.Array
                    ]
            )
        let solid2 = 
            IndexedGeometry(
                Mode = IndexedGeometryMode.TriangleList,
                IndexedAttributes =
                    SymDict.ofList [
                        DefaultSemantic.Positions, positions.ToArray() :> System.Array
                        Symbol.Create "KLMKind", coords.ToArray() :> System.Array
                    ]
            )

        solid, solid2

    


