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

type SvgNode =
    | Group of props : SvgProps * children : list<SvgNode>
    | Rect of props : SvgProps * w : Length * height : Length
    | Path of props : SvgProps * path : string // TODO
    | Text of props : SvgProps * components : list<Span>

    member x.Props =
        match x with
        | Group(p,_)
        | Rect(p,_,_)
        | Path(p,_)
        | Text(p,_) ->
            p

and Svg =
    {
        width       : Length
        height      : Length
        props       : SvgProps
        viewBox     : option<Box2d>
        elements    : list<SvgNode>
    }
