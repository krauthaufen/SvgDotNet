namespace SvgDotNet
        
open Aardvark.Base


type SizeUnit =
    | Pixel
    | Percent
    | Point
    | Picas
    | EM
    | ViewWidth
    | ViewHeight
    | Millimeter
    | Centimeter
    | Meter
    | Inch
    | Feet

type Length (value : float, unit : SizeUnit) =

    static member Zero = Length(0.0, SizeUnit.Pixel)

    member x.Value = value
    member x.Unit = unit

    // 1in = 96px
    member x.ToPixels(fontSize : float, total : float) =
        match unit with
        | SizeUnit.Percent | SizeUnit.ViewWidth | SizeUnit.ViewHeight -> 
            value * total / 100.0

        | SizeUnit.Pixel -> value
        | SizeUnit.Point -> value * 4.0 / 3.0
        | SizeUnit.Picas -> value * 16.0
        | SizeUnit.Millimeter -> value * 96.0 / 25.4
        | SizeUnit.Centimeter -> value * 96.0 / 2.54
        | SizeUnit.Meter -> value * 96.0 / 0.0254
        | SizeUnit.Inch -> value * 96.0
        | SizeUnit.Feet -> value * 1152.0
        | SizeUnit.EM -> value * fontSize

    override x.ToString() =
        let unit =
            match unit with
            | Picas -> "pc"
            | Pixel -> "px"
            | Percent -> "%"
            | Point -> "pt"
            | EM -> "em"
            | ViewWidth -> "vw"
            | ViewHeight -> "vh"
            | Millimeter -> "mm"
            | Centimeter -> "cm"
            | Meter -> "m"
            | Inch -> "in"
            | Feet -> "ft"
        sprintf "%g%s" value unit

    new(value) = Length(value, SizeUnit.Pixel)

type Transform =
    | Translate of x : Length * y : Length
    | Rotate of radians : float * center : option<Length * Length>
    | Scale of x: float * y: float
    | Skew of x: float * y: float
    | Matrix of M23d

type ValueOrSpecial<'a> =
    | Value of 'a
    | Special of string

type Value =
    | Color of C4b
    | Float of float
    | Angle of radians : float
    | Size of Length
    | Identifier of string
    | Unknown of string
    | Transform of list<Transform>


[<RequireQualifiedAccess>]
type Stroke =
    | Color of color : C4b
    | None
    | Inherit
    | Unspecified
    

[<RequireQualifiedAccess>]
type Fill =
    | Color of color : C4b
    | None
    | Inherit
    | Unspecified
    
[<RequireQualifiedAccess>]
type FontStyle =
    | Inherit
    | Unspecified
    | Normal
    | Italic
    | Bold
    | Oblique of angle : float

