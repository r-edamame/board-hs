
module Decoder exposing(..)

import Json.Decode as Decode exposing (Decoder)

import Types

decodeTopic : Decoder Types.Topic
decodeTopic =
    let
        title = Decode.field "title" Decode.string
        id = Decode.field "id" Decode.int
    in
        Decode.map2 Types.Topic title id


decodeTopics : Decoder (List Types.Topic)
decodeTopics =
    Decode.list decodeTopic

decodeComments : Decoder (List Types.Comment)
decodeComments =
    let
        text = Decode.field "comment" Decode.string
        id = Decode.field "topicId" Decode.int
    in
        Decode.map2 Types.Comment text id
        |> Decode.list
