
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
    Decode.field "comment" Decode.string
    |> Decode.map Types.Comment
    |> Decode.list
