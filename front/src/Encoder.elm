
module Encoder exposing (..)

import Json.Encode as Encode exposing (Value)

import Types exposing (Comment)

encodeComment : Comment -> Value
encodeComment comment =
    Encode.object 
        [ ("comment", Encode.string comment.comment)
        , ("topicId", Encode.int comment.topicId)
        ]

encodeNewTopic : String -> Value
encodeNewTopic title =
    Encode.object [("title", Encode.string title)]
