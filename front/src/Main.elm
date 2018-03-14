
module Main exposing (main)

import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events as Event

import Navigation as Nav

import Pages.NotFound as NotFound
import Pages.Home as Home
import Pages.Comments as Comments
import Pages.NewTopic as NewTopic

import Route exposing (Route)
import UrlParser as Url exposing (parsePath)

import Styles as S

type Msg
    = SetRoute (Maybe Route)
    | GotoHome
    | NotFoundMsg NotFound.Msg
    | HomeMsg Home.Msg
    | CommentsMsg Comments.Msg
    | NewTopicMsg NewTopic.Msg

type Page
    = NotFound
    | Home Home.Model
    | Comments Comments.Model
    | NewTopic NewTopic.Model

type alias Model =
    { page : Page }

init : Nav.Location -> (Model, Cmd Msg)
init location =
    { page=NotFound }
    |> setRoute (parsePath Route.route location)

setRoute : Maybe Route -> Model -> (Model, Cmd Msg)
setRoute mroute model =
    case mroute of
        Nothing -> ({ model | page=NotFound }, Cmd.none)
        Just Route.Home ->
            ({ model | page=Home Home.init }, Cmd.map HomeMsg Home.getTopics)
        Just (Route.Comments topicId) ->
            let (cmodel,ccmd) = Comments.initWithId topicId
            in
                ({ model | page=Comments cmodel }, Cmd.map CommentsMsg ccmd)
        Just Route.NewTopic ->
            let (nmodel, ncmd) = NewTopic.init
            in
                ({ model | page=NewTopic nmodel }, Cmd.none)

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
        , S.pointer
        ]
    

view : Model -> Html Msg
view model =
    let
        innerView = case model.page of
            NotFound -> Html.map NotFoundMsg NotFound.view
            Home hmodel -> Html.map HomeMsg (Home.view hmodel)
            Comments cmodel -> Html.map CommentsMsg (Comments.view cmodel)
            NewTopic nmodel -> Html.map NewTopicMsg (NewTopic.view nmodel)
    in
        Html.div [ S.bodyStyle ]
            [ Html.div [ titleStyle, Event.onClick GotoHome ] [ Html.text "Board" ]
            , innerView
            ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case (msg, model.page) of
        (SetRoute route, _) ->
            setRoute route model
        (GotoHome, _) ->
            setRoute (Just Route.Home) model
        (NotFoundMsg NotFound.Back, NotFound) ->
            (model, Nav.back 1)
        (HomeMsg hMsg, Home hModel) ->
            let (newModel, newCmd) = Home.update hMsg hModel
            in
                ({ model | page=Home newModel}, Cmd.map HomeMsg newCmd)
        (CommentsMsg cMsg, Comments cModel) ->
            let (newModel, newCmd) = Comments.update cMsg cModel
            in
                ({ model | page=Comments newModel }, Cmd.map CommentsMsg newCmd)
        (NewTopicMsg nMsg, NewTopic nModel) ->
            let (newModel, newCmd) = NewTopic.update nMsg nModel
            in
                ({ model | page=NewTopic newModel}, Cmd.map NewTopicMsg newCmd)
        (_, _) ->
            (model, Cmd.none)

main =
    Nav.program
        (parsePath Route.route >> SetRoute)
        { init=init
        , update=update
        , view=view
        , subscriptions=always Sub.none
        }
