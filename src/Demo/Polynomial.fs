namespace Aardvark.Base



[<Struct>]
type Complex(real : float, imag : float) =

    static let zeroRx = System.Text.RegularExpressions.Regex @"^(\-)?0\.0+$"

    member x.Real = real
    member x.Imaginary = imag

    member x.Conjugate = Complex(real, -imag)
    member x.NormSquared = real*real + imag*imag
    member x.Norm = sqrt (real*real + imag*imag)

    member x.Radius = x.Norm
    member x.Phi = atan2 imag real

    member x.IsReal(eps : float) = Fun.IsTiny(imag, eps)
    member x.IsImaginary(eps : float) = Fun.IsTiny(real, eps)
        
    member x.IsReal() = Fun.IsTiny(imag)
    member x.IsImaginary() = Fun.IsTiny(real)

    member x.IsZero(eps : float) = Fun.IsTiny(real, eps) && Fun.IsTiny(imag, eps)
    member x.IsZero() = Fun.IsTiny(real) && Fun.IsTiny(imag)
        
    member x.ToString(fmt : string) =
        let i = imag.ToString(fmt, System.Globalization.CultureInfo.InvariantCulture)
        let r = real.ToString(fmt, System.Globalization.CultureInfo.InvariantCulture)
        let iz = zeroRx.IsMatch i
        let rz = zeroRx.IsMatch r

        if iz && rz then r.TrimStart('-')
        elif iz then  r
        elif rz then i + "i"
        elif i.StartsWith "-" then sprintf "(%s%si)" r i
        else sprintf "(%s+%si)" r i


    override x.ToString() =
        let eps = 1E-12
        if Fun.IsTiny(imag,eps) then
            if Fun.IsTiny(real, eps) then "0"
            else sprintf "%g" real
        elif Fun.IsTiny(real,eps) then
            if Fun.ApproximateEquals(imag, 1.0, eps) then "i"
            elif Fun.ApproximateEquals(imag, -1.0, eps) then "-i"
            else sprintf "%gi" imag
        elif imag > 0.0 then 
            if Fun.ApproximateEquals(imag, 1.0, eps) then sprintf "(%g+i)" real
            else sprintf "(%g+%gi)" real imag
        else
            if Fun.ApproximateEquals(imag, -1.0, eps) then sprintf "(%g-i)" real
            else sprintf "(%g-%gi)" real -imag

    static member FromRadial(r : float, phi : float) =
        Complex(r*cos phi, r*sin phi)

    static member Zero = Complex(0.0, 0.0)
    static member One = Complex(1.0, 0.0)
    static member I = Complex(0.0, 1.0)

    static member (+) (l : Complex, r : Complex) = Complex(l.Real + r.Real, l.Imaginary + r.Imaginary)
    static member (+) (l : Complex, r : float) = Complex(l.Real + r, l.Imaginary)
    static member (+) (l : float, r : Complex) = Complex(l + r.Real, r.Imaginary)
        
    static member (-) (l : Complex, r : Complex) = Complex(l.Real - r.Real, l.Imaginary - r.Imaginary)
    static member (-) (l : Complex, r : float) = Complex(l.Real - r, l.Imaginary)
    static member (-) (l : float, r : Complex) = Complex(l - r.Real, -r.Imaginary)

    static member (*) (l : Complex, r : Complex) = Complex(l.Real*r.Real - l.Imaginary*r.Imaginary, l.Imaginary * r.Real + l.Real * r.Imaginary)
    static member (*) (l : Complex, r : float) = Complex(l.Real*r, l.Imaginary*r)
    static member (*) (l : float, r : Complex) = Complex(l*r.Real, l*r.Imaginary)
        
    static member (/) (l : Complex, r : Complex) = (l * r.Conjugate) / r.NormSquared
    static member (/) (l : Complex, r : float) = Complex(l.Real/r, l.Imaginary/r)
    static member (/) (l : float, r : Complex) = Complex(l*r.Real, -l*r.Imaginary) / r.NormSquared

    static member (~-) (l : Complex) = Complex(-l.Real, -l.Imaginary)

    static member Sqrt(c : Complex) = 
        Complex.FromRadial(sqrt c.Radius, c.Phi / 2.0)

    static member Sqrt(c : float) =
        if c >= 0.0 then Complex (sqrt c)
        else Complex(0.0, sqrt -c)

    static member Log(c : Complex) =
        Complex(log c.Norm, c.Phi)

    static member Pow(c : Complex, e : float) =
        Complex.FromRadial(c.Radius ** e, c.Phi * e)

    static member Pow(c : Complex, e : Complex) =
        if c.IsZero() then 
            Complex.Zero
        elif e.IsZero() then
            Complex.One
        else
            let l = e * Complex.Log c
            Complex.FromRadial(exp l.Real, l.Imaginary)
                
    static member Exp(c : Complex) =
        Complex.FromRadial(exp c.Real, c.Imaginary)

    static member Sin(c : Complex) =
        Complex(sin c.Real * cosh c.Imaginary, cos c.Real * sinh c.Imaginary)
            
    static member Cos(c : Complex) =
        Complex(cos c.Real * cosh c.Imaginary, -sin c.Real * sinh c.Imaginary)
            
    static member Tan(c : Complex) =
        Complex.Sin c / Complex.Cos c
            
    static member Sinh(c : Complex) =
        Complex(sinh c.Real * cos c.Imaginary, cosh c.Real * sin c.Imaginary)
            
    static member Cosh(c : Complex) =
        Complex(cosh c.Real * cos c.Imaginary, sinh c.Real * sin c.Imaginary)
            
    static member Tanh(c : Complex) =
        Complex.Sinh c / Complex.Cosh c
            
    new(real : float) = Complex(real, 0.0)

module Polynomial =
    let private normalize (eps : float) (poly: float[]) =
        let mutable i = poly.Length
        while i > 0 && Fun.IsTiny(poly.[i - 1], eps) do
            i <- i - 1

        if i > 0 then
            let res = Array.zeroCreate i
            for j in 0 .. i - 1 do
                res.[j] <- poly.[j]
            res
        else
            [||]
        
    
    let private randomFloats = 
        let rand = RandomSystem()
        Array.init 1024 (fun _ -> rand.UniformDouble())

    let eval (x : Complex) (poly : float[]) =
        let mutable c = Complex.One
        let mutable s = Complex.Zero
        for p in poly do
            s <- s + c * p
            c <- c * x
        s

    let private rootNormBounds (poly : float[]) =
        let n = poly.Length - 1

        let zassenhaus =
            let mutable vl = 0.0
            let mutable vp = 0.0
            let mutable mm = 0.0
            for i in 0 .. n do
                let e = n - i
                let v = 
                    if e = 1 then abs (poly.[i] / poly.[n])
                    else abs (poly.[i] / poly.[n]) ** (1.0 / float e)

                if v > vl then
                    vp <- vl
                    vl <- v
                elif v > vp then
                    vp <- v

            vp + vl

        let eval (s : float) =
            let mutable lagrange = 0.0
            let mutable cauchy = 0.0
            for i in 0 .. n - 1 do
                let f = s ** float (i - n + 1) 
                let r = abs (poly.[i] / poly.[n]) * f
                lagrange <- lagrange + r
                cauchy <- max cauchy r
            min (max s lagrange) (s + cauchy)
            
        let lagrangeCauchy = 
            [ 0.001; 0.01; 0.1; 1.0; 10.0; 100.0; 1000.0; 10000.0]
            |> List.map eval

        List.min (zassenhaus :: lagrangeCauchy)


    let roots (eps : float) (poly : float[]) =
        let normalized = normalize eps poly
        if normalized.Length <= 1 then
            0, []
        elif normalized.Length = 2 then
            0, [Complex (-normalized.[0] / normalized.[1])]
        elif normalized.Length = 3 then
            let c = normalized.[0]
            let b = normalized.[1]
            let a = normalized.[2]

            let q = Complex.Sqrt(b*b - 4.0*a*c)
            let d = 2.0 * a
            let t0 = (-b + q) / d
            let t1 = (-b - q) / d

            if t0.Real > t1.Real then 0, [t1; t0]
            else 0, [t0; t1]
        else
            let n = normalized.Length - 1
            let upperBound = rootNormBounds normalized
            let lowerBound = 1.0 / upperBound
            let boundSize = upperBound - lowerBound
            
            let mutable sumSq = 0.0
            let roots = 
                Array.init n (fun i -> 
                    let bi = i <<< 1
                    let r = lowerBound + randomFloats.[bi % 1024] * boundSize
                    let phi = randomFloats.[bi + 1 % 1024] * Constant.PiTimesTwo
                    sumSq <- sumSq + (r*r)
                    Complex.FromRadial(r, phi)
                )

            let p' = Polynomial.Derivative normalized

            let inline evalPolys (x : Complex) =
                let mutable c = Complex.One
                let mutable v = Complex.Zero
                let mutable v' = Complex.Zero

                for i in 0 .. n - 1 do
                    v <- v + c * normalized.[i]
                    v' <- v' + c * p'.[i]
                    c <- c * x

                v <- v + c * normalized.[n]
                struct(v, v')

            let mutable iter = 0
            let mutable converged = false
            let eps2 = eps * eps
            while not converged && iter < 500 do
                converged <- true

                let oldAvgSqNorm = sumSq / float normalized.Length
                sumSq <- 0.0
                for k in 0 .. roots.Length - 1 do
                    let struct (f, f') = evalPolys roots.[k]
                    let d = f / f'

                    let mutable s = Complex.Zero
                    for j in 0 .. roots.Length - 1 do
                        if j <> k then
                            s <- s + 1.0 / (roots.[k] - roots.[j])

                    let step = d / (1.0 - d*s)

                    if step.NormSquared / oldAvgSqNorm > eps2 then converged <- false

                    //if not (step.IsZero eps) then converged <- false
                    roots.[k] <- roots.[k] - step
                    sumSq <- sumSq + roots.[k].NormSquared

                iter <- iter + 1

            roots |> Array.sortInPlaceBy (fun v -> v.Real)
            if not converged then
                System.Int32.MaxValue , Array.toList roots
            else
                iter, Array.toList roots

    let realRoots (eps : float) (poly : float[]) =
        let res, list = roots eps poly 
        res, list |> List.choose (fun x ->
            if x.IsReal eps then Some x.Real
            else None
        )