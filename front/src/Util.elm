
module Util exposing (..)

import Html exposing (Html)

viewIf : Bool -> Html msg -> Html msg
viewIf cond html =
    case cond of
        True -> html
        False -> Html.text ""

(=>) : a -> b -> (a, b)
(=>) a b = (a, b)
