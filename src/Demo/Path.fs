﻿namespace Tessellator

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

            let intersectsConvex (eps : float) (l : Polygon2d) (r : Polygon2d) =
                let tess = Tess()

                let c0 = System.Collections.Generic.List<ContourVertex>()
                for p in l.Points do c0.Add(ContourVertex(Vec3(X = p.X, Y = p.Y)))
                c0.Add(c0.[0])
                
                let c1 = System.Collections.Generic.List<ContourVertex>()
                for p in r.Points do c1.Add(ContourVertex(Vec3(X = p.X, Y = p.Y)))
                c1.Add(c1.[0])
                tess.AddContour(c0, ContourOrientation.CounterClockwise)
                tess.AddContour(c1, ContourOrientation.CounterClockwise)
                tess.Tessellate(WindingRule.AbsGeqTwo, ElementType.BoundaryContours)

                let mutable empty = true
                let mutable ei = 0
                for _ in 0 .. tess.ElementCount - 1 do
                    let o = tess.Elements.[ei]
                    let c = tess.Elements.[ei+1]

                    let mutable box = Box2d.Invalid
                    let mutable vi = o
                    for i in 0 .. c - 1 do
                        let v = tess.Vertices.[vi]
                        let p = V2d(v.Position.X, v.Position.Y)
                        box.ExtendBy p
                        vi <- vi + 1

                    if box.Size.NormMax > eps then empty <- false

                    ei <- ei + 2


                not empty



            let private linesIntersect (eps : float) (l : Line2d) (r : Line2d) =
                let lr = l.Ray2d
                let rr = r.Ray2d
                let mutable t = 0.0
                if lr.Intersects(rr, &t) && t >= 1E-8 && t <= 1.0 - 1E-8 then
                    true
                else
                    false
                    
            let inline private sgn (eps : float) (v : float) =
                if v < -eps then -1
                elif v > eps then 1
                else 0

            let private polyAndLineIntersect (eps : float) (l : Polygon2d) (r : Line2d) =
                use e = l.EdgeLines.GetEnumerator()

                let mutable side0 = 0
                let mutable side1 = 0

                let mutable intersects = false
                while not intersects && e.MoveNext() do
                    let ll = e.Current

                    let v0 = r.P0.PosLeftOfLineValue(ll.P0, ll.P1) |> sgn eps
                    let v1 = r.P1.PosLeftOfLineValue(ll.P0, ll.P1) |> sgn eps

                    if side0 = 0 then side0 <- v0
                    elif side0 <> v0 then side0 <- 10
                        
                    if side1 = 0 then side1 <- v1
                    elif side1 <> v1 then side1 <- 10

                    if linesIntersect 1E-7 ll r then
                        intersects <- true

                intersects || (side0 = 1 || side0 = -1) || (side1 = 1 || side1 = -1)

                

            let overlaps (l : Part) (r : Part) =
                match l, r with
                | LinePart l, LinePart r -> linesIntersect 1E-7 l r
                | PolygonalPart(_,l), LinePart r -> polyAndLineIntersect 1E-7 l r
                | LinePart l, PolygonalPart(_,r) -> polyAndLineIntersect 1E-7 r l
                | PolygonalPart(_,l), PolygonalPart(_,r) -> 
                    intersectsConvex 1E-6 l r

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
            let b = Box2d.FromCenterAndSize(pt, V2d.II * 1E-3)
            bvh.GetIntersecting(b, fun _ _ t -> if Fun.ApproximateEquals(t.P0, pt) || Fun.ApproximateEquals(t.P1, pt) || Fun.ApproximateEquals(t.P2, pt) then Some t else None)
            |> HashMap.isEmpty
            |> not

        let boundary (rule : WindingRule) (parts : seq<#seq<V2d>>) =
            let tess = Tess()

            for path in parts do
                let vertices = System.Collections.Generic.List<ContourVertex>()

                for pt in path do
                    ContourVertex(Vec3(X = pt.X, Y = pt.Y)) |> vertices.Add

                tess.AddContour(vertices, ContourOrientation.Original)

            tess.Tessellate(rule, ElementType.BoundaryContours)
        
            let mutable i = 0
            let res = Array.zeroCreate tess.ElementCount
            for pi in 0 .. tess.ElementCount - 1 do
                let i0 = tess.Elements.[i]
                let cnt = tess.Elements.[i+1]
                let arr = Array.zeroCreate cnt

                let mutable oi = 0
                for vi in i0 .. i0 + cnt - 1 do
                    let v = tess.Vertices.[vi]
                    arr.[oi] <- V2d(v.Position.X, v.Position.Y)
                    oi <- oi + 1

                res.[pi] <- arr
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
            if tess.ElementCount > 0 then
                let positions = tess.Vertices |> Array.map (fun v -> V2d(v.Position.X, v.Position.Y))
                let index = tess.Elements
                positions, index
            else
                [||], [||]


    
    module private Coords =
        let bezier3 (q0 : V2d) (q1 : V2d) (q2 : V2d) (q3 : V2d) =
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
                    V4d(q0.X, q0.Y, 0.0, 1.0),
                    V4d(q1.X, q1.Y, 0.0, 1.0),
                    V4d(q2.X, q2.Y, 0.0, 1.0),
                    V4d(q3.X, q3.Y, 0.0, 1.0)
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
                    Choice2Of2(PathSegment.splitMany [a] (PathSegment.bezier3 q0 q1 q2 q3))
                elif vb then 
                    Choice2Of2(PathSegment.splitMany [b] (PathSegment.bezier3 q0 q1 q2 q3))
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
                match PathSegment.tryLine q0 q3 with
                | Some l -> Choice2Of2 [l]
                | None -> Choice2Of2 []

    /// tries to split the segment into two new segments having ranges [0;t] and [t;1] respectively.
    let split (t : float) (pt : V2d) (s : PathSegment) =
        if t <= 0.0 then (None, Some s)
        elif t >= 1.0 then (Some s, None)
        else
            match s with
            | Line(p0, p1) ->
                PathSegment.tryLine p0 pt, PathSegment.tryLine pt p1

            | Arc(p0, p1, a0, da, e) ->

                let l0 = a0
                let ld = da * t

                let r0 = a0 + ld
                let rd = da - ld

                let l = PathSegment.ForceArc(p0, pt, l0, ld, e)
                let r = PathSegment.ForceArc(pt, p1, r0, rd, e)

                Some l, Some r
                //let left =
                //    match tryArc l0 ld e with
                //    | Some(ArcSeg(_,_,a0,da,e)) -> Some (ArcSeg(p0, pm, a0, da, e))
                //    | Some(LineSeg _) -> Some (LineSeg(p0, pm))
                //    | o -> o

                //let right =
                //    match tryArc r0 rd  e with
                //    | Some(ArcSeg(_,_,a0,da,e)) -> Some (ArcSeg(pm, p1, a0, da, e))
                //    | Some(LineSeg _) -> Some (LineSeg(pm, p1))
                //    | o -> o

                //left, right


            | Bezier2(p0, p1, p2) ->
                let q0 = lerp p0 p1 t
                let q1 = lerp p1 p2 t
                PathSegment.tryBezier2 p0 q0 pt, PathSegment.tryBezier2 pt q1 p2

            | Bezier3(p0, p1, p2, p3) ->
                let q0 = lerp p0 p1 t
                let q1 = lerp p1 p2 t
                let q2 = lerp p2 p3 t
                let m0 = lerp q0 q1 t
                let m1 = lerp q1 q2 t
                PathSegment.tryBezier3 p0 q0 m0 pt, PathSegment.tryBezier3 pt m1 q2 p3


    /// splits the segment at the given parameter values and returns the list of resulting segments.
    let splitMany (ts : list<float * V2d>) (s : PathSegment) =
        match ts with
        | [] -> [s]
        | _ ->
            let rec run (ts : list<float * V2d>) (s : PathSegment) =
                match ts with
                | [] -> [s]
                | (t0, p0) :: ts ->
                    if t0 <= 0.0 then run ts s
                    elif t0 >= 1.0 then [s]
                    else
                        let ts = ts |> List.map (fun (t,p) -> (t-t0)/(1.0-t0), p)
                        let l, r = split t0 p0 s

                        let r = 
                            match r with
                            | Some r -> run ts r
                            | None -> []
                        match l with
                        | Some l -> l :: r
                        | None -> r
            run ts s



    let splitManyMany (l : PathSegment) (intersections : seq<'a * PathSegment * list<float*float>>) : list<PathSegment> * HashMap<'a, list<PathSegment>> =
        let intersections = Seq.toList intersections

        let all = 
            intersections 
            |> List.collect (fun (id, r, ts) -> ts |> List.map (fun (tl, tr) -> tl, (id, r, tr)))
            |> List.sortBy fst

        let rec run (splits : HashMap<'a, PathSegment * Map<float, V2d>>) (interactions : list<float * ('a * PathSegment * float)>) (l : PathSegment) =
            match interactions with
            | [] -> 
                [l], splits
            | (tl, (ri, r, tr)) :: rest ->
                let l0, l1 = PathSegment.split tl l

                match l1 with 
                | Some l1 ->
                
                    let pt = PathSegment.startPoint l1
                    let inline update (old : option<PathSegment * Map<float, V2d>>) =
                        match old with
                        | Some (s, m) ->
                            Some (s, Map.add tr pt m)
                        | None ->
                            Some(r, Map.ofList [tr, pt])
                        
                    let splits = HashMap.alter ri update splits
                    let rest = rest |> List.map (fun (ti, r) -> (ti - tl) / (1.0 - tl), r)

                    let result, splits = run splits rest l1
                    match l0 with
                    | Some l0 -> l0 :: result, splits
                    | None -> result, splits
                | None ->
                    match l0 with
                    | Some l0 -> [l0], splits
                    | None -> [], splits

        let self, result = run HashMap.empty all l

        let others = 
            result |> HashMap.map (fun _ (s, m) ->
                splitMany (Map.toList m) s
            )

        self, others

    /// assumptions:
    /// 1. the path is not self-intersecting
    let toGeometry (rule : WindingRule) (path : seq<PathSegment>) =
        let rule = 
            match rule with
            | WindingRule.Positive -> WindingRule.Negative
            | WindingRule.Negative -> WindingRule.Positive
            | r -> r

        let eps = 1E-9

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
                | Choice2Of2 replacement ->
                    Some replacement
                | _ ->
                    None
            | Arc(p0, p1, alpha0, dAlpha, ellipse) ->

                let t0 = ellipse.GetPoint(alpha0)
                let t1 = ellipse.GetPoint(alpha0 + dAlpha)

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

        // get rid of intersections
        let rec nonIntersecting (result : IndexList<PathSegment>) (bvh : BvhTree2d<Index, PathSegment>) (stack : list<PathSegment>) =

            let replace (i : Index) (repl : seq<PathSegment>) (l : IndexList<PathSegment>) (bvh : BvhTree2d<Index, PathSegment>) =
                use e = repl.GetEnumerator()
                if e.MoveNext() then
                    let _,_,r = IndexList.neighbours i l
                    let indexAfter =     
                        match r with
                        | Some(ri,_) -> fun i -> Index.between i ri
                        | None -> Index.after

                    let mutable b = BvhTree2d.remove i bvh
                    let mutable i = i
                    let mutable l = l |> IndexList.set i e.Current
                    b <- BvhTree2d.add i (PathSegment.bounds e.Current) e.Current b
                    while e.MoveNext() do
                        i <- indexAfter i
                        l <- IndexList.set i e.Current l
                        b <- BvhTree2d.add i (PathSegment.bounds e.Current) e.Current b

                    l, b
                else    
                    let l1 = IndexList.remove i l
                    let b1 = BvhTree2d.remove i bvh
                    l1, b1

            match stack with
            | s :: stack ->
                let b = PathSegment.bounds s
                
                let intersecting =  
                    let teps = 1E-7
                    bvh.GetIntersecting(b, fun _ _ other -> 
                        let all = PathSegment.intersections 1E-9 s other
                        let parameters = 
                            all
                            |> List.filter (fun (ta, tb) -> (ta >= teps && ta <= 1.0-teps) || (tb >= teps && tb <= 1.0-teps))

                        match parameters with
                        | [] -> None
                        | ts -> Some (other, ts)
                    )

                if HashMap.isEmpty intersecting then
                    let idx = Index.after result.MaxIndex
                    let result1 = IndexList.set idx s result
                    let bvh1 = bvh.Add(idx, b, s)
                    nonIntersecting result1 bvh1 stack
                else
                    
                    let self, others = splitManyMany s (HashMap.toSeq intersecting |> Seq.map (fun (a,(b,c)) -> a,b,c))

                    //let mine = 
                    //    intersecting 
                    //    |> HashMap.toList
                    //    |> List.collect (fun (o, (_, ts)) -> ts |> List.map fst)
                    //    |> List.sort

                    //let self = s |> PathSegment.splitMany mine
                    
                    let mutable r = result
                    let mutable b = bvh
                    for (oi, res) in others do
                        let (r1, b1) = replace oi res r b
                        r <- r1
                        b <- b1
                        

                    //let mutable r = result
                    //let mutable b = bvh
                    //for (oi, (o, ts)) in intersecting do
                    //    let ts = ts |> List.map snd
                    //    let res = PathSegment.splitMany ts o
                    //    let (r1, b1) = replace oi res r b
                    //    r <- r1
                    //    b <- b1


                    nonIntersecting r b (self @ stack)

            | [] ->
                result, bvh

        let path1,_ = nonIntersecting IndexList.empty BvhTree2d.empty (Seq.toList path)


        // get rid of overlapping triangulations
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

        let cleaned = nonOverlapping IndexList.empty BvhTree2d.empty (Seq.toList path1)

        // find closed sub-paths
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
            parts |> Seq.choose (fun (angle,path) ->
                if path.Count > 0 then
                    let first = Seq.head path
                    let rest = path |> Seq.map (fun p -> Part.endPoint p)
                    Seq.append (Seq.singleton (Part.startPoint first)) rest |> Some
                else
                    None
            )
            |> boundary rule
            |> triangulate WindingRule.Positive

        let nonCurved =
            nonCurvedTriangles
            |> toBvh

        let edges, boundary = 
            let tess = Tess()
            let polygons = System.Collections.Generic.List<PathSegment * Polygon2d>()
            for (pi, (exterior, path)) in IndexList.toSeqIndexed parts do
                if not (IndexList.isEmpty path) then
                    let contour = System.Collections.Generic.List<ContourVertex>()
                
                    let inline approx (c : ContourVertex) (v : V2d) =
                        Fun.ApproximateEquals(c.Position.X, v.X, eps) && Fun.ApproximateEquals(c.Position.Y, v.Y, eps)


                    let add (v : V2d) (s : seq<Index * int>) =
                        let ks = s |> Seq.map (fun (a,b) -> pi,a,b) |> Set.ofSeq
                        contour.Add(ContourVertex(Vec3(X = v.X, Y = v.Y), ks))

                    let mutable li = path.MaxIndex
                    let mutable lastPart = Unchecked.defaultof<_>

                    //let (s0, part) = Seq.head (IndexList.toSeqIndexed path)
                    //let p0 = Part.startPoint part
                    //add p0 [li,1; s0,0]


                    for (si, part) in IndexList.toSeqIndexed path do
                        match part with
                        | LinePart l ->
                            add l.P0 [li,1; si,0]
                            //add l.P1 si [si,1; si,0]
                        | PolygonalPart(s, p) ->
                            match s with
                            | Line(p0, p1) ->
                                add p0 [li,1; si,0]
                                //add p1 si 1

                            | Bezier2(p0, p1, p2) ->
                                add p0 [li,1; si,0]
                                if containsPoint nonCurved p1 then add p1 []
                                //add p2 si 1
                            | Bezier3(p0, p1, p2, p3) ->
                                add p0 [li,1; si,0]
                                if containsPoint nonCurved p1 then add p1 []
                                if containsPoint nonCurved p2 then add p2 []
                                //add p3 si 1
                            | Arc(p0, p2, alpha0, dAlpha, ellipse) ->
                                let p1 = ellipse.GetControlPoint(alpha0, alpha0 + dAlpha)
                                add p0 [li,1; si,0]
                                if containsPoint nonCurved p1 then add p1 []
                                //add p2 si 1
                        li <- si
                        lastPart <- part

                    contour.Add(contour.[0])

                    if contour.Count > 0 then
                        tess.AddContour(contour)

             
            let interpolate _ (l : obj[]) _ =
                if l.Length = 1 then l.[0]
                else l |> Seq.map unbox<Set<Index*Index*int>> |> Set.unionMany :> obj

            tess.Tessellate(rule, ElementType.BoundaryContours, combineCallback = CombineCallback(interpolate))

            let mutable edges = Set.empty
            let res = Array.zeroCreate tess.ElementCount
            let mutable ei = 0
            for i in 0 .. tess.ElementCount - 1 do  
                let o = tess.Elements.[ei]
                let c = tess.Elements.[ei+1]
                let arr = Array.zeroCreate c

                let mutable last = tess.Vertices.[o + c-1]
                let mutable vi = o
                for j in 0 .. c - 1 do
                    let v = tess.Vertices.[vi]

                    let i0 = last.Data |> unbox<Set<Index * Index * int>>
                    let i1 = v.Data |> unbox<Set<Index * Index * int>>

                    for l in i0 do
                        for r in i1 do
                            edges <- edges |> Set.add (l, r)

                    arr.[j] <- V2d(v.Position.X, v.Position.Y)
                    vi <- vi + 1
                    last <- v

                ei <- ei + 2
                res.[i] <- arr
            edges, res


        let polygons = System.Collections.Generic.List()
        for (pi, (_,path)) in IndexList.toSeqIndexed parts do
            for (si, s) in IndexList.toSeqIndexed path do
                match s with
                | PolygonalPart(s, _) ->
                    let p0 = PathSegment.startPoint s
                    let p1 = PathSegment.endPoint s
                    if hasPoint nonCurved p0 && hasPoint nonCurved p1 then
                    //if Set.contains ((pi, si, 0),(pi, si, 1)) edges then
                        polygons.Add s
                | _ ->
                    ()






        let pos, idx = 
            boundary
            |> triangulate WindingRule.Positive

        let positions = 
            idx |> Array.map (fun i -> V3f(V2f pos.[i], 0.0f)) |> System.Collections.Generic.List

        let coords = 
            Array.init positions.Count (fun _ -> V4f.Zero) |> System.Collections.Generic.List

        for seg in polygons do 
            match seg with
            | Line _ -> 
                ()

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
                    

            | Bezier3(p0, p1, p2, p3) ->
                match Coords.bezier3 p0 p1 p2 p3 with
                | Choice1Of2(k, c0, c1, c2, c3) ->
                    
                    let c1Inside = c1.X*c1.X*c1.X - c1.Y*c1.Z < 0.0
                    let c2Inside = c2.X*c2.X*c2.X - c2.Y*c2.Z < 0.0

                    let inline flip (v : V3d) = V3d(-v.XY, v.Z)

                    positions.AddRange [ V3f(V2f p0, 0.0f); V3f(V2f p1, 0.0f); V3f(V2f p2, 0.0f) ]
                    positions.AddRange [ V3f(V2f p0, 0.0f); V3f(V2f p2, 0.0f); V3f(V2f p3, 0.0f) ]

                    if containsPoint nonCurved p1 <> c1Inside then
                        coords.AddRange [V4f(flip c0, float k); V4f(flip c1, float k); V4f(flip c2,float k)]
                        coords.AddRange [V4f(flip c0, float k); V4f(flip c2, float k); V4f(flip c3,float k)]
                    else
                        coords.AddRange [V4f(c0, float k); V4f(c1, float k); V4f(c2,float k)]
                        coords.AddRange [V4f(c0, float k); V4f(c2, float k); V4f(c3,float k)]
                        
                | _ -> 
                    failwith "should have been subdivided"




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

    


