
module Pages.NewTopic exposing (..)

import Html exposing (Html)
import Html.Attributes as Att exposing (style)
import Html.Events as Event

import Http
import HttpBuilder as HB

import Route exposing (routeToString)
import Navigation as Nav

import Decoder exposing (decodeTopic)
import Encoder exposing (encodeNewTopic)

import Types exposing (Topic)

import Styles as S

type alias Model =
    { input : String
    , error : String
    }

init : (Model, Cmd Msg)
init = ({ input="", error="" }, Cmd.none)

type Msg
    = ChangeInput String
    | Submit
    | ReceiveTopic (Result Http.Error Topic)
    | Back

-- styles
backButtonStyle =
    S.backButtonBase
    |> (++)
        [ S.marginLeft 10
        ]
    |> style
formStyle =
    style
        [ S.widthPer 90.0
        , S.center
        , S.marginTop 20
        ]
submitButtonStyle =
    style S.submitButtonBase

view : Model -> Html Msg
view model =
    Html.div []
        [ Html.button [ backButtonStyle, Event.onClick Back ] [ Html.text "back" ]
        , Html.div [ formStyle ]
            [ Html.input [ Att.placeholder "new topic", Event.onInput ChangeInput ] []
            , Html.button [ submitButtonStyle,  Event.onClick Submit ] [ Html.text "create" ]
            ]
        ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ChangeInput input ->
            ({ model | input=input }, Cmd.none)
        Submit ->
            (model, postTopic model.input)
        ReceiveTopic (Ok topic) ->
            (model, Route.Comments topic.id |> routeToString |> Nav.newUrl)
        ReceiveTopic (Err error) ->
            ({ model | error=toString error }, Cmd.none)
        Back ->
            (model, Nav.back 1)

postTopic : String -> Cmd Msg
postTopic title =
    "/" ++ String.join "/" ["api", "topics", "create"]
    |> HB.post
    |> HB.withJsonBody (encodeNewTopic title)
    |> HB.withExpectJson decodeTopic
    |> HB.send ReceiveTopic
