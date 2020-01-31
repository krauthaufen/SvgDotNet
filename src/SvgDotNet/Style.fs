﻿namespace SvgDotNet
        
open Aardvark.Base
open SvgDotNet


type Style =
    {
        transform       : list<Transform>
        stroke          : Stroke
        strokeWidth     : option<Length>
        fill            : Fill

        fontFamily      : option<string>
        fontSize        : option<Length>
        fontStyle       : FontStyle
        letterSpacing   : option<Length>
        wordSpacing     : option<Length>

        other           : Map<string, list<Value>>
    }

    static member (+) (l : Style, r : Style) =  
        let opt a b =
            match b with
            | Some b -> Some b
            | None -> a

        let mutable res = l.other
        for KeyValue(k,v) in r.other do
            res <- Map.add k v res

        {
            transform = r.transform
            stroke =
                match r.stroke with
                | Stroke.Inherit | Stroke.Unspecified -> l.stroke
                | _ -> r.stroke

            fill = 
                match r.fill with
                | Fill.Inherit | Fill.Unspecified -> l.fill
                | _ -> r.fill

            strokeWidth = opt l.strokeWidth r.strokeWidth

            fontFamily = opt l.fontFamily r.fontFamily
            fontSize = opt l.fontSize r.fontSize
            fontStyle =
                match r.fontStyle with
                | FontStyle.Unspecified | FontStyle.Inherit -> l.fontStyle
                | _ -> r.fontStyle
            letterSpacing = opt l.letterSpacing r.letterSpacing
            wordSpacing = opt l.wordSpacing r.wordSpacing

            other = res
        }

module Style =
    let none =
        {       
            transform = []
            stroke = Stroke.Unspecified
            strokeWidth = None
            fill = Fill.Unspecified
            other = Map.empty
            fontFamily = None
            fontSize = None
            fontStyle = FontStyle.Unspecified
            letterSpacing = None
            wordSpacing = None
        }

    let initial =
        {       
            transform = []
            stroke = Stroke.None
            strokeWidth = Some (Length 1.0)
            fill = Fill.Color C4b.Black
            other = Map.empty
            fontFamily = Some "Times New Roman"
            fontSize = Some (Length 16.0)
            fontStyle = FontStyle.Normal
            letterSpacing = None
            wordSpacing = None
        }

    let ofMap (map : Map<string, list<Value>>) =  

        let mutable map = map

        let inline tryRemove name =
            match Map.tryFind name map with
            | Some v ->
                map <- Map.remove name map
                Some v
            | None ->
                None

        let stroke = 
            match tryRemove "stroke" with
            | Some [Color c] -> Stroke.Color c
            | Some [Identifier "none"] -> Stroke.None
            | Some [Identifier "inherit"] -> Stroke.Inherit
            | Some other -> failwithf "bad stroke: %A" other
            | None -> Stroke.Unspecified
  
        let strokeWidth = 
            match tryRemove "stroke-width" with
            | Some [Size l] -> Some l
            | Some [Float v] -> Some(Length(v,SizeUnit.Point))
            | Some [Identifier "none"] -> Some(Length(0.0,SizeUnit.Point))
            | Some other -> failwithf "bad stroke-width: %A" other
            | None -> None
                
        let fill = 
            match tryRemove "fill" with
            | Some [Color c] -> Fill.Color c
            | Some [Identifier "none"] -> Fill.None
            | Some [Identifier "inherit"] -> Fill.Inherit
            | Some other -> failwithf "bad fill: %A" other
            | None -> Fill.Unspecified

        let fontFamily =
            match tryRemove "font-family" with
            | Some l ->
                let name = l |> List.choose (function Identifier s | Unknown s -> Some s | _ -> None) |> String.concat " "
                Some name
            | None ->
                None

        let fontStyle =
            match tryRemove "font-style" with
            | Some [Identifier "normal"] -> FontStyle.Normal
            | Some [Identifier "italic"] -> FontStyle.Italic
            | Some [Identifier "bold"] -> FontStyle.Bold
            | Some [Identifier "oblique"] -> FontStyle.Italic
            | Some [Identifier "oblique"; Float angle] -> FontStyle.Oblique angle
            | Some other -> failwithf "bad font-style: %A" other
            | None -> FontStyle.Unspecified

        let fontSize =
            match tryRemove "font-size" with
            | Some [Float v] -> Some (Length v)
            | Some [Size l] -> Some l
            | Some other -> failwithf "bad font-size: %A" other
            | None -> None
            
        let letterSpacing =
            match tryRemove "letter-spacing" with
            | Some [Float v] -> Some (Length v)
            | Some [Size l] -> Some l
            | Some other -> failwithf "bad letter-spacing: %A" other
            | None -> None
            
        let wordSpacing =
            match tryRemove "word-spacing" with
            | Some [Float v] -> Some (Length v)
            | Some [Size l] -> Some l
            | Some other -> failwithf "bad word-spacing: %A" other
            | None -> None


        let transform =
            match tryRemove "transform" with
            | Some [Transform l] -> l
            | _ -> []

        {
            transform = transform
            stroke = stroke
            strokeWidth = strokeWidth
            fill = fill
            fontFamily = fontFamily
            fontStyle = fontStyle
            fontSize = fontSize
            wordSpacing = wordSpacing
            letterSpacing = letterSpacing
            other = map
        }
