
module Pages.NotFound exposing (view, Msg (..))

import Html exposing (Html)
import Html.Attributes as Att exposing (style)
import Html.Events as Event

import Navigation exposing (modifyUrl)

import Styles as S

type Msg
    = Back

bodyStyle =
    style
        [ S.widthPer 90.0
        , S.center
        ]
backButtonStyle =
    style S.backButtonBase

view : Html Msg
view =
    Html.div [ bodyStyle ] 
        [ Html.div [] [ Html.text "Not Found" ]
        , Html.button [ backButtonStyle, Event.onClick Back ] [ Html.text "back" ]
        ]
