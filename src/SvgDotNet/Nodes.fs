namespace SvgDotNet

open Aardvark.Base
open SvgDotNet
    
type SvgProps =
    {
        id          : option<string>
        x           : Length
        y           : Length
        style       : Style
    }

module SvgProps =

    let empty =
        { 
            id = None
            x = Length.Zero
            y = Length.Zero
            style = Style.none
        }


type Span =
    {
        props   : SvgProps
        content : string
    }

type SvgNode =
    //| Root of svg : Svg
    | Group of props : SvgProps * children : list<SvgNode>
    | Rect of props : SvgProps * w : Length * height : Length
    | Path of props : SvgProps * path : string // TODO
    | Text of props : SvgProps * components : list<Span>

    member x.Props =
        match x with
        //| Root svg -> svg.props
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
