
module Pages.Comments exposing (..)

import Html exposing (Html)
import Html.Attributes as Att exposing (style)
import Html.Events as Event

import Http
import HttpBuilder as HB
import Navigation as Nav

import Types exposing (Comment)

import Decoder exposing (decodeComments, decodeTopic)
import Encoder exposing (encodeComment)

import Styles as S

import Util as U

type alias Model =
    { topicId : Int
    , topicTitle : String
    , comments : List Comment
    , commentInput : String
    , error : String
    }

initWithId : Int -> (Model, Cmd Msg)
initWithId id =
    let model = {topicId=id, topicTitle="title", comments=[], commentInput="", error=""}
    in
     (model, getTopic model)

-- styles
errorStyle =
    style
        [ S.color "red"
        ]
commentsStyle =
    style
        [ S.center
        , S.widthPer 90.0
        , S.backGroundColor "white"
        , S.marginTop 10
        ]
ulStyle =
    style
        [ S.center
        , S.decimalListStyleType
        , S.listStyleInside
        , S.noPadding
        ]
titleStyle =
    style
        [ S.fontSize 28
        , S.borderStyle S.Bottom "solid"
        , S.borderWidth S.Bottom 3
        ]
commentStyle =
    style
        [ S.fontSize 20
        , S.marginTop 10
        ]
backButtonStyle =
    S.backButtonBase
    |> (++)
        [ S.marginLeft 10
        ]
    |> style
formStyle =
    style
        [ S.center
        , S.widthPer 90.0 
        , S.marginTop 20
        , S.marginBottom 20
        ]
submitStyle =
    S.submitButtonBase
    |> style

view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div []
            [ Html.button [ backButtonStyle, Event.onClick Back ] [ Html.text "back" ]
            ]
        , Html.div [ errorStyle ] [ Html.text model.error ]
            |> U.viewIf (String.isEmpty model.error |> not)
        , Html.div [ commentsStyle ]
            [ Html.div [ titleStyle ] [ Html.text model.topicTitle ]
            , Html.ul [ ulStyle ] <| List.map showComment model.comments
            ]
        , Html.div [ formStyle ]
            [ Html.div [] 
                [ Html.input [ Event.onInput ChangeInput, Att.placeholder "comment", Att.value model.commentInput ] []
                , Html.button [ submitStyle, Event.onClick SubmitComment ] [ Html.text "submit" ]
                ]
            , Html.div [] [ Html.button [ submitStyle, Event.onClick Reload ] [ Html.text "reload" ] ]
            ]
        ]

showComment : Comment -> Html msg
showComment comment =
    Html.li [ commentStyle ] [ Html.text comment.comment ]

type Msg
    = ReceiveComments (Result Http.Error (List Comment))
    | ReceiveTopic (Result Http.Error Types.Topic)
    | ChangeInput String
    | SubmitComment
    | Reload
    | Back

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ReceiveComments (Ok comments) ->
            ({ model | error="", comments=comments }, Cmd.none)
        ReceiveComments (Err error) ->
            ({ model | error=toString error }, Cmd.none)
        ReceiveTopic (Ok topic) ->
            ({ model | topicTitle=topic.title, error="" }, getComments model)
        ReceiveTopic (Err error) ->
            ({ model | error=toString error }, Cmd.none)
        ChangeInput input ->
            ({ model | commentInput=input }, Cmd.none)
        SubmitComment ->
            ({ model | commentInput="" }, postComment model (Comment model.commentInput))
        Reload ->
            (model, getComments model) 
        Back ->
            (model, Nav.back 1)

getComments : Model -> Cmd Msg
getComments model =
    HB.get "/api/comments"
    |> HB.withQueryParams [("topic_id", toString model.topicId)]
    |> HB.withExpectJson decodeComments
    |> HB.send ReceiveComments

postComment : Model -> Comment -> Cmd Msg
postComment model comment =
    "/" ++ String.join "/" [ "api", "comments", toString model.topicId ]
    |> HB.post 
    |> HB.withJsonBody (encodeComment comment)
    |> HB.withExpectJson decodeComments
    |> HB.send ReceiveComments

getTopic : Model -> Cmd Msg
getTopic model =
    HB.get "/api/topic"
    |> HB.withQueryParams [("topic_id", toString model.topicId)]
    |> HB.withExpectJson decodeTopic
    |> HB.send ReceiveTopic
