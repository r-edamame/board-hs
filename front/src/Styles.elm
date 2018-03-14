
module Styles exposing (..)

import Html exposing (Attribute)
import Html.Attributes exposing (style)

import Util exposing ((=>))

type alias Style = (String, String)

type Side
    = Top
    | Right
    | Bottom
    | Left
    | All

sideToString : Side -> String
sideToString side =
    case side of
        Top -> "-top"
        Right -> "-right"
        Bottom -> "-bottom"
        Left -> "-left"
        All -> ""

px : Int -> String
px p = toString p ++ "px"

per : Float -> String
per p = toString p ++ "%"

center : Style
center = "margin" => "auto"

widthPer : Float -> Style
widthPer p = "width" => per p

widthPix : Int -> Style
widthPix p = "width" => px p

marginTop : Int -> Style
marginTop p = "margin-top" => px p

marginBottom : Int -> Style
marginBottom p = "margin-bottom" => px p

marginLeft : Int -> Style
marginLeft p = "margin-left" => px p

marginRight : Int -> Style
marginRight p = "margin-right" => px p

paddingTop : Int -> Style
paddingTop p = "padding-top" => px p

paddingBottom : Int -> Style
paddingBottom p = "padding-bottom" => px p

borderStyle : Side -> String -> Style
borderStyle side style =
    String.concat ["border", sideToString side, "-style"]
    => style

borderColor : Side -> String -> Style
borderColor side color =
    String.concat ["border", sideToString side, "-color"]
    => color

borderWidth : Side -> Int -> Style
borderWidth side width =
    String.concat ["border", sideToString side, "-width"]
    => px width

noPadding : Style
noPadding = "padding" => "0"

fontSize : Int -> Style
fontSize s = "font-size" => px s

centerText : Style
centerText = "text-align" => "center"

color : String -> Style
color col = "color" => col

float : String -> Style
float a = "float" => a

pointer : Style
pointer = "cursor" => "pointer"

backGroundColor : String -> Style
backGroundColor col = "background-color" => col

nonListStyleType : Style
nonListStyleType = "list-style-type" => "none"

decimalListStyleType : Style
decimalListStyleType = "list-style-type" => "decimal"

listStyleInside : Style
listStyleInside = "list-style-position" => "inside"

bodyStyle =
    style
        [ center
        --, S.widthPer 80.0
        , widthPix 300
        , backGroundColor "#eeeeee"
        , marginTop 50
        , paddingTop 20
        , paddingBottom 20
        ]

backButtonStyle =
    style
        [ backGroundColor "gray"
        ]
buttonBase = [ pointer ]

submitButtonBase = backGroundColor "#64c8fa" :: buttonBase

backButtonBase = backGroundColor "gray" :: buttonBase
