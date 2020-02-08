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

    let private flip (a,b) = b,a

    
    let private findBezierT (epsilon : float) (pt : V2d) (p0 : V2d) (p1 : V2d) (p2 : V2d) : option<float> =
        let a = p0 - 2.0*p1 + p2
        let b = 2.0*p1 - 2.0*p0
        let c = p0 - pt

        let struct(t0, t1) = Polynomial.RealRootsOf(a.X, b.X, c.X)
        let struct(t2, t3) = Polynomial.RealRootsOf(a.Y, b.Y, c.Y)

        let inline check t = if t >= -epsilon && t <= 1.0 + epsilon then Some t else None

        if Fun.ApproximateEquals(t0, t2, epsilon) then check ((t0 + t2) / 2.0)
        elif Fun.ApproximateEquals(t0, t3, epsilon) then check ((t0 + t3) / 2.0)
        elif Fun.ApproximateEquals(t1, t2, epsilon) then check ((t1 + t2) / 2.0)
        elif Fun.ApproximateEquals(t1, t3, epsilon) then check ((t1 + t3) / 2.0)
        else None

        //let v0 = t0 >= -epsilon && t0 <= 1.0 + epsilon
        //let v1 = t1 >= -epsilon && t1 <= 1.0 + epsilon

        //if v0 && v1 then Some (min t0 t1)
        //elif v0 then Some t0
        //elif v1 then Some t1
        //else None



    let internal angleDifference (a0 : float) (a1 : float) =
        let d1 = a1 - a0
        let d2 = d1 - Constant.PiTimesTwo
        let d3 = d1 + Constant.PiTimesTwo

        if abs d1 < abs d2 then 
            if abs d1 < abs d3 then d1
            else d3
        else 
            if abs d2 < abs d3 then d2
            else d3

    let private intersectArcAndLine (epsilon : float) (alpha0: float) (alpha1 : float) (e : Ellipse2d) (a0 : V2d) (b0 : V2d) =
        // transform the ellipse to a unit-circle and solve the system in that space.
        let toGlobal = M33d.FromCols(V3d(e.Axis0, 0.0), V3d(e.Axis1, 0.0), V3d(e.Center, 1.0))
        let toLocal = toGlobal.Inverse

        let r = Ray2d(a0, b0 - a0)

        let p0 = toLocal.TransformPos a0
        let p1 = toLocal.TransformPos b0
        let o = p0
        let d = p1 - p0

        // | o + t*d | = 1
        // <o+t*d|o+t*d> = 1
        // <o|o+t*d> + t*<d|o+t*d> = 1
        // <o|o> + 2t*<o|d> + t^2*<d|d> = 1
        // t^2*(<d|d>) + t*(2*<o|d>) + (o^2 - 1) = 0
        let a = Vec.lengthSquared d
        let b = 2.0 * Vec.dot o d
        let c = Vec.lengthSquared o - 1.0

        let q2 = b*b - 4.0*a*c
            
        let inline test (t : float) =
            if t >= -epsilon && t <= 1.0 + epsilon then
                let pt = r.GetPointOnRay t 
                let a = e.GetAlpha(pt)
                let ta = angleDifference alpha0 a / angleDifference alpha0 alpha1 //(alpha1 - alpha0)
                if ta >= -epsilon && ta <= 1.0 + epsilon then [(ta, t)]
                else []
            else
                []

        if Fun.IsTiny q2 then
            test (-b / (2.0 * a))

        elif q2 < 0.0 then
            []
        else
            let q = sqrt q2
            let t1 = (-b + q) / (2.0 * a)
            let t2 = (-b - q) / (2.0 * a)
            test t1 @ test t2

    let private intersectBezier2AndLine (epsilon : float) (p0 : V2d) (p1 : V2d) (p2 : V2d) (q0 : V2d) (q1 : V2d) =
        // p0*(1-t0)^2 + p1*t0*(1-t0) + p2*t0^2 = o + t1*d

        // p0.X*(1-t0)^2 + p1.X*t0*(1-t0) + p2.X*t0^2 = o.X + t1*d.X
        // p0.Y*(1-t0)^2 + p1.Y*t0*(1-t0) + p2.Y*t0^2 = o.Y + t1*d.Y
        
        //  p0.X*d.Y*(1-t0)^2 + p1.X*d.Y*t0*(1-t0) + p2.X*d.Y*t0^2 =  o.X*d.Y + t1*d.X*d.Y
        // -p0.Y*d.X*(1-t0)^2 - p1.Y*d.X*t0*(1-t0) - p2.Y*d.X*t0^2 = -o.Y*d.X - t1*d.X*d.Y
        
        // p0.X*d.Y*(1-t0)^2 + p1.X*d.Y*t0*(1-t0) + p2.X*d.Y*t0^2 - p0.Y*d.X*(1-t0)^2 - p1.Y*d.X*t0*(1-t0) - p2.Y*d.X*t0^2 = o.X*d.Y - o.Y*d.X
        

        // RHS = o.X*d.Y - o.Y*d.X

        //  p0.X*d.Y*(1 - 2*t0 + t0^2)
        // +p1.X*d.Y*t0 
        // -p1.X*d.Y*t0^2
        // -p2.X*d.Y*t0^2
        // -p0.Y*d.X*(1 - 2*t0 + t0^2)
        // -p1.Y*d.X*t0
        // +p1.Y*d.X*t0^2
        // -p2.Y*d.X*t0^2


        //  p0.X*d.Y
        // -2*t0*p0.X*d.Y
        // +p0.X*d.Y*t0^2
        // +p1.X*d.Y*t0 
        // -p1.X*d.Y*t0^2
        // -p2.X*d.Y*t0^2
        // -p0.Y*d.X
        // +2*t0*p0.Y*d.X
        // -p0.Y*d.X*t0^2
        // -p1.Y*d.X*t0
        // +p1.Y*d.X*t0^2
        // -p2.Y*d.X*t0^2
        

        let o = q0
        let d = q1 - q0
        let dp0 = d.X*p0.Y - d.Y*p0.X
        let dp1 = d.X*p1.Y - d.Y*p1.X
        let dp2 = d.X*p2.Y - d.Y*p2.X
        let do0 = d.X*o.Y - d.Y*o.X

        let a = dp0 - dp1 + dp2
        let b = -2.0 * dp0 - dp1
        let c = dp0 - do0
        
        // p0*(1-t)^2 + p1*t*(1-t) + p2*t^2 = pt
        // p0 - 2*p0*t + p0*t^2 + p1*t - p1*t^2 + p2*t^2 = pt
        
        // t^2*(p0 - p1 + p2) + t*(p1 - 2*p0) + (p0 - pt) = 0

        let inline test (tl : float) =
            if tl >= -epsilon && tl <= 1.0 + epsilon then
                let pt = o + d * tl
                match findBezierT epsilon pt p0 p1 p2 with
                | Some tb -> [tb, tl]
                | None -> []
            else
                []

        
        let q2 = sqr b -  4.0 * a * c
        if Fun.IsTiny(q2) then
            test (-b / (2.0 * a))
        elif q2 < 0.0 then
            []
        else
            let q = sqrt q2
            let t0 = (-b + q) / (2.0 * a)
            let t1 = (-b - q) / (2.0 * a)
            test t0 @ test t1

    let private intersectNumeric (epsilon : float) (a : PathSegment) (b : PathSegment) =
        let inline eval (t : V2d) =
            let pa = PathSegment.point t.X a
            let pb = PathSegment.point t.Y b
            let da = PathSegment.derivative t.X a
            let db = PathSegment.derivative t.Y b
            let ca = PathSegment.secondDerivative t.X a
            let cb = PathSegment.secondDerivative t.Y b
                
            // f(t1, t2) = (a(t1) - b(t2))^2
            // df(t1, t2)/dt1 = 2*(a(t1) - b(t2))*a'(t1)
            // df(t1, t2)/dt1^2 = 2*a'(t)*a'(t) + 2*(a(t1)-b(t2))*a''(t)
            // df(t1, t2)/dt1*dt2 = -2*b'(t2)*a'(t1)
            // df(t1, t2)/dt2 = 2*(a(t1) - b(t2))*-b'(t2)
            // df(t1, t2)/dt2*dt1 = -2*a'(t1)*b'(t2)
            // df(t1, t2)/dt2^2 = 2*b'(t2)*b'(t2) + 2*(a(t1) - b(t2))*b''(t)

            let f = 
                Vec.lengthSquared (pa - pb)

            let f' =
                let v = 2.0*(pa-pb)
                V2d(Vec.dot v da, Vec.dot v -db)

            let dab = Vec.dot db da
            let f'' =
                M22d(
                     2.0 * Vec.dot da da + 2.0 * Vec.dot (pa-pb) ca,
                    -2.0 * dab,
                    -2.0 * dab,
                     2.0 * Vec.dot db db + 2.0 * Vec.dot (pa-pb) cb
                )

            struct(f, f', f'')

        let mutable iter = 0
        let mutable tLast = V2d.Zero
        let mutable t = V2d(0.2131414, 0.5435)
        let mutable r = struct(System.Double.PositiveInfinity, V2d.Zero, M22d.Identity)

        while not (Fun.ApproximateEquals(tLast, t, 1E-7)) && iter < 200 do
            tLast <- t
            let struct(f, f', f'') = eval t
            t <- t - f''.Inverse * f'
            r <- struct(f, f', f'')
            iter <- iter + 1

                
        let struct(r,_,_) = r
        if Fun.IsTiny(r, epsilon) && t.X >= -epsilon && t.X <= 1.0 + epsilon && t.Y >= -epsilon && t.Y <= 1.0 + epsilon then
            Some (t.X, t.Y)
        else
            None

    let private intersectBezier2 (epsilon : float) (p0 : V2d) (p1 : V2d) (p2 : V2d) (q0 : V2d) (q1 : V2d) (q2 : V2d) =
        let f0 =
           -4.0*q0.Y*q1.X*q1.Y*q2.X + 4.0*q0.X*sqr(q1.Y)*q2.X + sqr(q0.Y)*sqr(q2.X) + sqr(p0.Y)*sqr(q0.X - 2.0*q1.X + q2.X) + 4.0*q0.Y*sqr(q1.X)*q2.Y - 4.0*q0.X*q1.X*q1.Y*q2.Y - 2.0*q0.X*q0.Y*q2.X*q2.Y + sqr(q0.X)*sqr(q2.Y) + sqr(p0.X)*sqr(q0.Y - 2.0*q1.Y + q2.Y) - 
            2.0*p0.Y*(-2.0*q0.X*q1.X*q1.Y + 4.0*q0.X*q1.Y*q2.X - 2.0*q1.X*q1.Y*q2.X + q0.Y*(2.0*sqr(q1.X) - q0.X*q2.X - 2.0*q1.X*q2.X + sqr(q2.X)) + sqr(q0.X)*q2.Y - 2.0*q0.X*q1.X*q2.Y + 2.0*sqr(q1.X)*q2.Y - q0.X*q2.X*q2.Y + p0.X*(q0.X - 2.0*q1.X + q2.X)*(q0.Y - 2.0*q1.Y + q2.Y)) + 
            2.0*p0.X*(-(sqr(q0.Y)*q2.X) + 2.0*q1.Y*(-(q1.Y*q2.X) + q1.X*q2.Y) + q0.Y*(2.0*q1.Y*q2.X + 2.0*q1.X*(q1.Y - 2.0*q2.Y) + (q0.X + q2.X)*q2.Y) - q0.X*(2.0*sqr(q1.Y) - 2.0*q1.Y*q2.Y + sqr(q2.Y)))
        
        let f1 =
            -4.0*(2.0*p1.Y*q0.Y*sqr(q1.X) - 2.0*p1.Y*q0.X*q1.X*q1.Y - 2.0*p1.X*q0.Y*q1.X*q1.Y + 2.0*p1.X*q0.X*sqr(q1.Y) - p1.Y*q0.X*q0.Y*q2.X + p1.X*sqr(q0.Y)*q2.X - 2.0*p1.Y*q0.Y*q1.X*q2.X + 4.0*p1.Y*q0.X*q1.Y*q2.X - 2.0*p1.X*q0.Y*q1.Y*q2.X - 2.0*p1.Y*q1.X*q1.Y*q2.X + 2.0*p1.X*sqr(q1.Y)*q2.X + 
                 p1.Y*q0.Y*sqr(q2.X) + sqr(p0.Y)*sqr(q0.X - 2.0*q1.X + q2.X) + p1.Y*sqr(q0.X)*q2.Y - p1.X*q0.X*q0.Y*q2.Y - 2.0*p1.Y*q0.X*q1.X*q2.Y + 4.0*p1.X*q0.Y*q1.X*q2.Y + 2.0*p1.Y*sqr(q1.X)*q2.Y - 2.0*p1.X*q0.X*q1.Y*q2.Y - 2.0*p1.X*q1.X*q1.Y*q2.Y - p1.Y*q0.X*q2.X*q2.Y - p1.X*q0.Y*q2.X*q2.Y + 
                 p1.X*q0.X*sqr(q2.Y) + sqr(p0.X)*sqr(q0.Y - 2.0*q1.Y + q2.Y) + p0.Y*(p1.X*q0.X*q0.Y - 2.0*p1.X*q0.Y*q1.X - 2.0*q0.Y*sqr(q1.X) - 2.0*p1.X*q0.X*q1.Y + 4.0*p1.X*q1.X*q1.Y + 2.0*q0.X*q1.X*q1.Y + p1.X*q0.Y*q2.X + q0.X*q0.Y*q2.X + 2.0*q0.Y*q1.X*q2.X - 2.0*p1.X*q1.Y*q2.X - 4.0*q0.X*q1.Y*q2.X + 
                    2.0*q1.X*q1.Y*q2.X - q0.Y*sqr(q2.X) - p1.Y*sqr(q0.X - 2.0*q1.X + q2.X) + p1.X*q0.X*q2.Y - sqr(q0.X)*q2.Y - 2.0*p1.X*q1.X*q2.Y + 2.0*q0.X*q1.X*q2.Y - 2.0*sqr(q1.X)*q2.Y + p1.X*q2.X*q2.Y + q0.X*q2.X*q2.Y - 2.0*p0.X*(q0.X - 2.0*q1.X + q2.X)*(q0.Y - 2.0*q1.Y + q2.Y)) + 
                 p0.X*(2.0*q0.Y*q1.X*q1.Y - 2.0*q0.X*sqr(q1.Y) - sqr(q0.Y)*q2.X + 2.0*q0.Y*q1.Y*q2.X - 2.0*sqr(q1.Y)*q2.X + q0.X*q0.Y*q2.Y - 4.0*q0.Y*q1.X*q2.Y + 2.0*q0.X*q1.Y*q2.Y + 2.0*q1.X*q1.Y*q2.Y + q0.Y*q2.X*q2.Y - q0.X*sqr(q2.Y) + p1.Y*(q0.X - 2.0*q1.X + q2.X)*(q0.Y - 2.0*q1.Y + q2.Y) - 
                    p1.X*sqr(q0.Y - 2.0*q1.Y + q2.Y)))

        let f2 =
          2.0*(-(p0.X*p2.Y*q0.X*q0.Y) + 3.0*sqr(p0.X)*sqr(q0.Y) - 6.0*p0.X*p1.X*sqr(q0.Y) + 2.0*sqr(p1.X)*sqr(q0.Y) + p0.X*p2.X*sqr(q0.Y) + 2.0*p0.X*p2.Y*q0.Y*q1.X - 2.0*p2.Y*q0.Y*sqr(q1.X) + 2.0*p0.X*p2.Y*q0.X*q1.Y - 12.0*sqr(p0.X)*q0.Y*q1.Y + 24.0*p0.X*p1.X*q0.Y*q1.Y - 
             8.0*sqr(p1.X)*q0.Y*q1.Y - 4.0*p0.X*p2.X*q0.Y*q1.Y - 4.0*p0.X*p2.Y*q1.X*q1.Y + 2.0*p2.Y*q0.X*q1.X*q1.Y + 2.0*p0.X*q0.Y*q1.X*q1.Y - 4.0*p1.X*q0.Y*q1.X*q1.Y + 2.0*p2.X*q0.Y*q1.X*q1.Y + 12.0*sqr(p0.X)*sqr(q1.Y) - 24.0*p0.X*p1.X*sqr(q1.Y) + 8.0*sqr(p1.X)*sqr(q1.Y) + 
             4.0*p0.X*p2.X*sqr(q1.Y) - 2.0*p0.X*q0.X*sqr(q1.Y) + 4.0*p1.X*q0.X*sqr(q1.Y) - 2.0*p2.X*q0.X*sqr(q1.Y) - p0.X*p2.Y*q0.Y*q2.X + p2.Y*q0.X*q0.Y*q2.X - p0.X*sqr(q0.Y)*q2.X + 2.0*p1.X*sqr(q0.Y)*q2.X - p2.X*sqr(q0.Y)*q2.X + 2.0*p2.Y*q0.Y*q1.X*q2.X + 2.0*p0.X*p2.Y*q1.Y*q2.X - 
             4.0*p2.Y*q0.X*q1.Y*q2.X + 2.0*p0.X*q0.Y*q1.Y*q2.X - 4.0*p1.X*q0.Y*q1.Y*q2.X + 2.0*p2.X*q0.Y*q1.Y*q2.X + 2.0*p2.Y*q1.X*q1.Y*q2.X - 2.0*p0.X*sqr(q1.Y)*q2.X + 4.0*p1.X*sqr(q1.Y)*q2.X - 2.0*p2.X*sqr(q1.Y)*q2.X - p2.Y*q0.Y*sqr(q2.X) + 3.0*sqr(p0.Y)*sqr(q0.X - 2.0*q1.X + q2.X) + 
             2.0*sqr(p1.Y)*sqr(q0.X - 2.0*q1.X + q2.X) - p0.X*p2.Y*q0.X*q2.Y - p2.Y*sqr(q0.X)*q2.Y + 6.0*sqr(p0.X)*q0.Y*q2.Y - 12.0*p0.X*p1.X*q0.Y*q2.Y + 4.0*sqr(p1.X)*q0.Y*q2.Y + 2.0*p0.X*p2.X*q0.Y*q2.Y + p0.X*q0.X*q0.Y*q2.Y - 2.0*p1.X*q0.X*q0.Y*q2.Y + p2.X*q0.X*q0.Y*q2.Y + 
             2.0*p0.X*p2.Y*q1.X*q2.Y + 2.0*p2.Y*q0.X*q1.X*q2.Y - 4.0*p0.X*q0.Y*q1.X*q2.Y + 8.0*p1.X*q0.Y*q1.X*q2.Y - 4.0*p2.X*q0.Y*q1.X*q2.Y - 2.0*p2.Y*sqr(q1.X)*q2.Y - 12.0*sqr(p0.X)*q1.Y*q2.Y + 24.0*p0.X*p1.X*q1.Y*q2.Y - 8.0*sqr(p1.X)*q1.Y*q2.Y - 4.0*p0.X*p2.X*q1.Y*q2.Y + 2.0*p0.X*q0.X*q1.Y*q2.Y - 
             4.0*p1.X*q0.X*q1.Y*q2.Y + 2.0*p2.X*q0.X*q1.Y*q2.Y + 2.0*p0.X*q1.X*q1.Y*q2.Y - 4.0*p1.X*q1.X*q1.Y*q2.Y + 2.0*p2.X*q1.X*q1.Y*q2.Y - p0.X*p2.Y*q2.X*q2.Y + p2.Y*q0.X*q2.X*q2.Y + p0.X*q0.Y*q2.X*q2.Y - 2.0*p1.X*q0.Y*q2.X*q2.Y + p2.X*q0.Y*q2.X*q2.Y + 3.0*sqr(p0.X)*sqr(q2.Y) - 6.0*p0.X*p1.X*sqr(q2.Y) + 
             2.0*sqr(p1.X)*sqr(q2.Y) + p0.X*p2.X*sqr(q2.Y) - p0.X*q0.X*sqr(q2.Y) + 2.0*p1.X*q0.X*sqr(q2.Y) - p2.X*q0.X*sqr(q2.Y) - 
             p0.Y*(6.0*p0.X*q0.X*q0.Y - 6.0*p1.X*q0.X*q0.Y + p2.X*q0.X*q0.Y - 12.0*p0.X*q0.Y*q1.X + 12.0*p1.X*q0.Y*q1.X - 2.0*p2.X*q0.Y*q1.X + 2.0*q0.Y*sqr(q1.X) - 12.0*p0.X*q0.X*q1.Y + 12.0*p1.X*q0.X*q1.Y - 2.0*p2.X*q0.X*q1.Y + 24.0*p0.X*q1.X*q1.Y - 24.0*p1.X*q1.X*q1.Y + 4.0*p2.X*q1.X*q1.Y - 2.0*q0.X*q1.X*q1.Y + 
                6.0*p0.X*q0.Y*q2.X - 6.0*p1.X*q0.Y*q2.X + p2.X*q0.Y*q2.X - q0.X*q0.Y*q2.X - 2.0*q0.Y*q1.X*q2.X - 12.0*p0.X*q1.Y*q2.X + 12.0*p1.X*q1.Y*q2.X - 2.0*p2.X*q1.Y*q2.X + 4.0*q0.X*q1.Y*q2.X - 2.0*q1.X*q1.Y*q2.X + q0.Y*sqr(q2.X) + 6.0*p1.Y*sqr(q0.X - 2.0*q1.X + q2.X) - p2.Y*sqr(q0.X - 2.0*q1.X + q2.X) + 
                6.0*p0.X*q0.X*q2.Y - 6.0*p1.X*q0.X*q2.Y + p2.X*q0.X*q2.Y + sqr(q0.X)*q2.Y - 12.0*p0.X*q1.X*q2.Y + 12.0*p1.X*q1.X*q2.Y - 2.0*p2.X*q1.X*q2.Y - 2.0*q0.X*q1.X*q2.Y + 2.0*sqr(q1.X)*q2.Y + 6.0*p0.X*q2.X*q2.Y - 6.0*p1.X*q2.X*q2.Y + p2.X*q2.X*q2.Y - q0.X*q2.X*q2.Y) + 
             2.0*p1.Y*(2.0*q0.Y*sqr(q1.X) - 2.0*q0.X*q1.X*q1.Y - q0.X*q0.Y*q2.X - 2.0*q0.Y*q1.X*q2.X + 4.0*q0.X*q1.Y*q2.X - 2.0*q1.X*q1.Y*q2.X + q0.Y*sqr(q2.X) + sqr(q0.X)*q2.Y - 2.0*q0.X*q1.X*q2.Y + 2.0*sqr(q1.X)*q2.Y - q0.X*q2.X*q2.Y + 3.0*p0.X*(q0.X - 2.0*q1.X + q2.X)*(q0.Y - 2.0*q1.Y + q2.Y) - 
                2.0*p1.X*(q0.X - 2.0*q1.X + q2.X)*(q0.Y - 2.0*q1.Y + q2.Y)))

        let f3 =
            -4.0*(p2.Y*q0.X - p0.X*q0.Y + 2.0*p1.X*q0.Y - p2.X*q0.Y - 2.0*p2.Y*q1.X + 2.0*p0.X*q1.Y - 4.0*p1.X*q1.Y + 2.0*p2.X*q1.Y + p2.Y*q2.X + p0.Y*(q0.X - 2.0*q1.X + q2.X) - 2.0*p1.Y*(q0.X - 2.0*q1.X + q2.X) - p0.X*q2.Y + 2.0*p1.X*q2.Y - p2.X*q2.Y)*
                (p0.Y*(q0.X - 2.0*q1.X + q2.X) - p1.Y*(q0.X - 2.0*q1.X + q2.X) - (p0.X - p1.X)*(q0.Y - 2.0*q1.Y + q2.Y))
        
        let f4 = 
            sqr(-(p2.Y*q0.X) + p0.X*q0.Y - 2.0*p1.X*q0.Y + p2.X*q0.Y + 2.0*p2.Y*q1.X - 2.0*p0.X*q1.Y + 4.0*p1.X*q1.Y - 2.0*p2.X*q1.Y - p2.Y*q2.X - p0.Y*(q0.X - 2.0*q1.X + q2.X) + 2.0*p1.Y*(q0.X - 2.0*q1.X + q2.X) + p0.X*q2.Y - 2.0*p1.X*q2.Y + p2.X*q2.Y)

        let struct (t0, t1, t2, t3) = Polynomial.RealRootsOf(f4, f3, f2, f1, f0)

        let inline evalP (t : float) =
            if t >= -epsilon && t <= 1.0 + epsilon then
                let s = 1.0 - t
                Some (p0*s*s + 2.0*p1*s*t + p2*t*t)
            else
                None

        let inline evalQ (t : float) =
            if t >= -epsilon && t <= 1.0 + epsilon then
                let s = 1.0 - t
                Some (q0*s*s + 2.0*q1*s*t + q2*t*t)
            else
                None
                
        let test (tp : float) =
            match evalP tp with
            | Some pp ->
                match findBezierT epsilon pp q0 q1 q2 with
                | Some tq ->
                    match evalQ tq with
                    | Some pq ->
                        if Fun.ApproximateEquals(pq, pp, epsilon) then [tp, tq]
                        else []
                    | None ->
                        []
                | None ->
                    []
            | None ->
                []

        let res = test t0 @ test t1 @ test t2 @ test t3
        res

    let rec intersect (epsilon : float) (a : PathSegment) (b : PathSegment) : option<float * float> =
        match a, b with
        | Line(a0, a1), Line(b0, b1) ->
            let ra = Ray2d(a0, a1 - a0)
            let rb = Ray2d(b0, b1 - b0)
            let mutable ta = 0.0
            let mutable tb = 0.0
            if ra.Intersects(rb, &ta) && rb.Intersects(ra, &tb) && ta <= 1.0 - epsilon && ta >= epsilon && tb <= 1.0 - epsilon && tb >= epsilon then
                Some (ta, tb)
            else
                None

        | Line(a, b), Arc(alpha0, alpha1, e) ->
            match intersectArcAndLine epsilon alpha0 alpha1 e a b with
            | [] -> None
            | l -> l |> List.minBy snd |> flip |> Some

        | Arc(alpha0, alpha1, e), Line(a, b) ->
            match intersectArcAndLine epsilon alpha0 alpha1 e a b with
            | [] -> None
            | l -> l |> List.minBy fst |> Some
            
        | Line(a, b), Bezier2(p0, p1, p2) ->
            match intersectBezier2AndLine epsilon p0 p1 p2 a b with
            | [] -> None
            | l -> l |> List.minBy snd |> flip |> Some
            
        | Bezier2(p0, p1, p2), Line(a, b) ->
            match intersectBezier2AndLine epsilon p0 p1 p2 a b with
            | [] -> None
            | l -> l |> List.minBy fst |> Some

        | Bezier2(p0, p1, p2), Bezier2(q0, q1, q2) ->
            match intersectBezier2 epsilon p0 p1 p2 q0 q1 q2 with
            | [] -> None
            | l -> l |> List.minBy fst |> Some

        | a, b ->   
            failwithf "cannot intersect %A and %A" a b
            intersectNumeric epsilon a b

    let withT1 (t1 : float) (s : PathSegment) =
        if t1 <= 0.0 then
            None
        elif t1 >= 1.0 then
            Some s
        else
            match s with
            | Line(p0,_) -> 
                PathSegment.tryLine p0 (PathSegment.point t1 s)

            | Arc(alpha0, alpha1, e) ->
                let a1 = alpha0 + t1 * angleDifference alpha0 alpha1
                PathSegment.tryArc alpha0 a1 e

            | Bezier2(p0, _, _) ->
                // t=0 => d0 = 2.0 * (p1 - p0) => p1 = p0 + d0/2.0
                // t=1 => d1 = 2.0 * (p2 - p1) => p1 = p2 - d1/2.0
                let d = PathSegment.derivative t1 s
                let p = PathSegment.point t1 s
                let c = p - d / 2.0
                PathSegment.tryBezier2 p0 c p

            | Bezier3(p0, c0, _, _) ->
                let d = PathSegment.derivative t1 s
                let p = PathSegment.point t1 s
                let c1 = p - d / 3.0
                PathSegment.tryBezier3 p0 c0 c1 p

    let withT0 (t0 : float) (s : PathSegment) =
        if t0 >= 1.0 then
            None
        elif t0 <= 0.0 then
            Some s
        else
            match s with
            | Line(_,p1) -> 
                PathSegment.tryLine (PathSegment.point t0 s) p1

            | Arc(alpha0, alpha1, e) ->
                let a0 = alpha0 + t0 * angleDifference alpha0 alpha1
                PathSegment.tryArc a0 alpha1 e

            | Bezier2(_, _, p1) ->
                // t=0 => d0 = 2.0 * (p1 - p0) => p1 = p0 + d0/2.0
                // t=1 => d1 = 2.0 * (p2 - p1) => p1 = p2 - d1/2.0
                let d = PathSegment.derivative t0 s
                let p = PathSegment.point t0 s
                let c = p + d / 2.0
                PathSegment.tryBezier2 p c p1

            | Bezier3(_, _, c1, p1) ->
                let d = PathSegment.derivative t0 s
                let p = PathSegment.point t0 s
                let c0 = p + d / 3.0
                PathSegment.tryBezier3 p c0 c1 p1

    let withRange (t0 : float) (t1 : float) (s : PathSegment) =
        if t0 <= 0.0 && t1 >= 1.0 then
            Some s
        elif t0 <= 0.0 then
            withT1 t1 s
        elif t1 >= 1.0 then
            withT0 t0 s
        elif t0 < t1 then
            match s with
            | Line(_,_) -> 
                let p0 = PathSegment.point t0 s
                let p1 = PathSegment.point t1 s
                PathSegment.tryLine p0 p1

            | Arc(alpha0, alpha1, e) ->
                let d = angleDifference alpha0 alpha1
                let a0 = alpha0 + t0 * d
                let a1 = alpha0 + t1 * d
                PathSegment.tryArc a0 a1 e

            | Bezier2(p0, p1, p2) ->
                // t=0 => d0 = 2.0 * (p1 - p0) => p1 = p0 + d0/2.0
                // t=1 => d1 = 2.0 * (p2 - p1) => p1 = p2 - d1/2.0
                let p0 = PathSegment.point t0 s
                let p1 = PathSegment.point t1 s
                let d0 = PathSegment.derivative t0 s
                let d1 = PathSegment.derivative t1 s

                let c0 = p0 + d0 / 2.0
                let c1 = p1 - d1 / 2.0
                if not (Fun.ApproximateEquals(c0, c1, 1E-8)) then
                    failwithf "that was unexpected: %A vs %A" c0 c1

                PathSegment.tryBezier2 p0 c0 p1

            | Bezier3 _ ->
                let p0 = PathSegment.point t0 s
                let p1 = PathSegment.point t1 s
                let d0 = PathSegment.derivative t0 s
                let d1 = PathSegment.derivative t1 s

                //t=0 => d0 = 3.0 * (p1 - p0)
                //t=1 => d1 = 3.0 * (p3 - p2)
                // p1 = p0 + d0/3.0
                // p2 = p3 - d1/3.0

                let c0 = p0 + d0 / 3.0
                let c1 = p1 - d1 / 3.0
                PathSegment.tryBezier3 p0 c0 c1 p1
        else
            None

    let split (t : float) (s : PathSegment) =
        if t <= 0.0 then (None, Some s)
        elif t >= 1.0 then (Some s, None)
        else
            match s with
            | Line(p0, p1) ->
                let m = lerp p0 p1 t
                PathSegment.tryLine p0 m, PathSegment.tryLine m p1

            | Arc(alpha0, alpha1, e) ->
                let a = alpha0 + t * angleDifference alpha0 alpha1
                PathSegment.tryArc alpha0 a e, PathSegment.tryArc a alpha1 e

            | Bezier2(p0, _, p1) ->
                let m = PathSegment.point t s
                let dm = PathSegment.derivative t s / 2.0
                PathSegment.tryBezier2 p0 (m - dm) m, PathSegment.tryBezier2 m (m + dm) p1

            | Bezier3(p0, c0, c1, p1) ->
                let m = PathSegment.point t s
                let dm = PathSegment.derivative t s / 3.0

                PathSegment.tryBezier3 p0 c0 (m - dm) m, PathSegment.tryBezier3 m (m + dm) c1 p1


                

    let rec private tryClip (a : PathSegment) (b : PathSegment) : option<option<PathSegment> * option<PathSegment>> =
        let a1 = PathSegment.startPoint a
        let b0 = PathSegment.startPoint b
        if Fun.ApproximateEquals(a1, b0, 1E-8) then
            None
        else 
            match intersect 1E-8 a b with
            | Some (ta, tb) ->
                Some (withRange 0.0 ta a, withRange tb 1.0 b)
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

        let setStartPoint (p : V2d) (s : PathSegment) =
            match s with
            | Line(_,p1) -> Line(p, p1)
            | Bezier2(_,p1,p2) -> Bezier2(p, p1, p2)
            | Bezier3(_,p1,p2,p3) -> Bezier3(p, p1, p2, p3)
            | Arc(a0, a1, e) -> s
            
        let setEndPoint (p : V2d) (s : PathSegment) =
            match s with
            | Line(p0,_) -> Line(p0, p)
            | Bezier2(p0,p1,_) -> Bezier2(p0, p1, p)
            | Bezier3(p0,p1,p2,_) -> Bezier3(p, p1, p2, p)
            | Arc(a0, a1, e) -> s

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

            | Arc(a0, a1, ae), Arc(b0, b1, be) ->
                if Fun.ApproximateEquals(a1, b0) && Fun.ApproximateEquals(ae.Center, be.Center) && Fun.ApproximateEquals(ae.Axis0, be.Axis0) && Fun.ApproximateEquals(ae.Axis1, be.Axis1) then
                    Some (PathSegment.arc a0 b1 ae)
                else
                    None

            | _ ->
                // TODO: implement
                None

        member x.Add(segment : PathSegment) =
            if store.Count > 0 then 
                let li = store.Count - 1
                let last = store.[li]
                match tryMerge last segment with
                | Some s -> 
                    store.RemoveAt li
                    x.Add s
                | None ->
                    let a = PathSegment.endPoint last
                    let b = PathSegment.startPoint segment
                    if Fun.ApproximateEquals(a, b, eps) then   
                        store.Add (setStartPoint a segment)
                    else
                        match tryClip last segment with
                        | Some (nl, ns) ->
                            match nl, ns with
                            | Some nl, Some ns ->
                                store.[li] <- nl
                                store.Add ns
                            | None, Some n ->
                                store.RemoveAt li
                                x.Add n
                            | Some n, None ->
                                store.[li] <- n
                            | None, None ->
                                store.RemoveAt li
                        | None ->       
                            let add, keepA, addSeg = joinSegments style miterLimit last segment
                            if keepA then
                                for a in add do 
                                    match a with
                                    | Some a -> x.Add a
                                    | None -> ()
                            else
                                store.RemoveAt li

                            if addSeg then x.Add segment
                                
            else
                store.Add segment

        member x.Last =
            if store.Count > 0 then Some store.[store.Count - 1]
            else None

        member x.ToArray() =
            store.ToArray()

    let rec private appendOffset (w : float) (c : SegmentList) (p : PathSegment) =
        match p with
        | Line(p0, p1) ->
            let dir = p1 - p0
            let len = Vec.length dir
            let d = dir / len
            let n = V2d(-d.Y, d.X)
            match PathSegment.tryLine  (p0 + n * w) (p1 + n * w) with
            | Some l -> c.Add l
            | None -> ()

        | Arc(a, b, e) ->
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
                match withRange t0 t1 p with
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

    let private offsetPath (style : LineJoin) (miterLimit : float) (w : float) (p : Path) =
        let input = Path.toArray p
        let list = SegmentList(style, miterLimit, 1E-8)
        input |> Array.iter (appendOffset w list)
        let all = list.ToArray()
        
        let rand = RandomSystem()
        printfn "<g transform=\"translate(60,60)\" style=\"stroke-linejoin:bevel;stroke-linecap:butt;fill:none;stroke:black;stroke-width:1px\">"
        for s in all do
            match s with
            | Line(p0, p1) ->
                let c = rand.UniformC3f().ToC3b()
                printfn "  <line x1=\"%.5f\" y1=\"%.5f\" x2=\"%.5f\" y2=\"%.5f\" style=\"stroke:#%02X%02X%02X\" />" p0.X p0.Y p1.X p1.Y c.R c.G c.B
            | _ ->
                ()
        printfn "</g>"


        //let att = result |> Seq.choose (function Line(p0, p1) -> Some (sprintf "M %.5f,%.5f L %.5f %.5f" p0.X p0.Y p1.X p1.Y) | _ -> None) |> String.concat " "
        //printfn "<path transform=\"translate(60,60)\" style=\"stroke-linejoin:bevel;stroke-linecap:butt;fill:none;stroke:black;stroke-width:1px\" d=\"%s\" />" att 


        all






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
                let comp = offsetPath join miterLimit s path
                add (comp |> Array.toList |> List.map Some)


            add (
                let p = PathSegment.endPoint last
                let d = -PathSegment.tangent 1.0 last
                getCap cap p d s
            )
                
            do
                let comp = offsetPath join miterLimit s (Path.reverse path)
                add (comp |> Array.toList |> List.map Some)

            Path.ofSeq segments

        else
            Path.empty


[<EntryPoint;STAThread>]
let main argv = 
    PolynomialTest.run()
    System.Environment.Exit 0

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



    let path = @"C:\Users\Schorsch\Development\aardvark.rendering\data\logo\aardvark.svg" //Path.combine [__SOURCE_DIRECTORY__; ".."; ".."; "data"; "Example.svg"] //@"C:\Users\Schorsch\Downloads\Example_svg.svg"

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
