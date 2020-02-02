namespace SvgDotNet

open Aardvark.Base
open SvgDotNet
    
type SvgProps =
    {
        id          : option<string>
        x           : option<Length>
        y           : option<Length>
        dx          : option<Length>
        dy          : option<Length>
        style       : Style
    }

module SvgProps =

    let empty =
        { 
            id = None
            x = None
            y = None
            dx = None
            dy = None
            style = Style.none
        }


type Span =
    {
        props   : SvgProps
        content : string
    }

type SvgPathComponent =
    | Move of x : Length * y : Length


type V2L(x : Length, y : Length) =
    member __.X = x
    member __.Y = y


type SvgConstructor =
    | Group of children : list<SvgNode>
    | Rect of position : V2L * size : V2L
    | Circle of radius : Length * center : V2L
    | Ellipse of rx : Length * ry : Length * center : V2L
    | Line of p0 : V2L * p1 : V2L
    | Polygon of points : list<V2L>
    | Polyline of points : list<V2L>
    | Text of components : list<Span>
    | Path of path : list<SvgPathComponent>

and SvgNode =
    { 
        constructor : SvgConstructor
        props : SvgProps
    }
    //| Group of props : SvgProps * children : list<SvgNode>
    //| Rect of props : SvgProps * w : Length * height : Length
    //| Path of props : SvgProps * path : string // TODO
    //| Text of props : SvgProps * components : list<Span>

    //member x.Props =
    //    match x with
    //    | Group(p,_)
    //    | Rect(p,_,_)
    //    | Path(p,_)
    //    | Text(p,_) ->
    //        p

and Svg =
    {
        width       : Length
        height      : Length
        props       : SvgProps
        viewBox     : option<Box2d>
        elements    : list<SvgNode>
    }
