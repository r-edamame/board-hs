
module Pages.Home exposing (..)

import Types exposing (Topic)

import Html exposing (Html)
import Html.Attributes as Att exposing (style)
import Html.Events as Event

import Http
import HttpBuilder as HB

import Navigation as Nav

import Route exposing (Route)

import Decoder exposing (decodeTopics)

import Util as U

import Route exposing (routeToString, Route(..))

import Styles as S

type alias Model =
    { topics : List Topic
    , error : String
    }

init : Model
init = Model [] ""

-- styles
titleStyle =
    style
        [ S.fontSize 35
        , S.backGroundColor "white"
        , S.borderStyle S.Bottom "solid"
        , S.borderWidth S.Bottom 2
        , S.widthPer 90.0
        , S.center
        , S.marginBottom 10
        ]
buttonStyle =
    style
        [ S.backGroundColor "#57b5cc"
        , S.marginLeft 30
        , S.marginRight 30
        , S.float "right"
        ]
topicsStyle =
    style
        [ S.center
        , S.widthPer 80.0
        , S.fontSize 20
        , S.marginTop 50
        ]
ulStyle =
    style
        [ S.center
        , S.noPadding
        , S.nonListStyleType
        ]
errorStyle =
    style
        [ S.color "red"
        ]
topicStyle =
    style
        [ S.center
        , S.centerText
        , S.widthPer 80.0
        , S.backGroundColor "white"
        , S.marginTop 10
        , S.marginBottom 10
        , S.pointer
        ]

view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div []
            [ Html.button [ buttonStyle, Event.onClick NewTopic ] [ Html.text "new" ]
            ]
        , Html.div [ errorStyle ] [ Html.text model.error ] |> U.viewIf (String.isEmpty model.error |> not)
        , Html.div [ topicsStyle ]
            [ Html.div [] [ Html.text "all topics" ]
            , Html.ul [ ulStyle ] <| List.map showTopic model.topics
            ]
        ]

showTopic : Topic -> Html Msg
showTopic topic =
    Html.li [ topicStyle, SelectTopic topic.id |> Event.onClick ] [ Html.text topic.title ]

getTopics : Cmd Msg
getTopics =
    HB.get "/api/topics"
    |> HB.withExpectJson decodeTopics
    |> HB.send ReceiveTopics


type Msg
    = ReceiveTopics (Result Http.Error (List Topic))
    | FlownAway
    | SelectTopic Int
    | NewTopic

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ReceiveTopics (Ok topics) ->
            ({ model | topics=topics }, Cmd.none )
        ReceiveTopics (Err error) ->
            ({ model | error=toString error }, Cmd.none)
        FlownAway -> (model, Nav.newUrl "/test")
        SelectTopic topicId ->
            (model, routeToString (Comments topicId) |> Nav.newUrl)
        NewTopic ->
            (model, routeToString Route.NewTopic |> Nav.newUrl)
