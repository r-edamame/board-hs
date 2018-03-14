
module Requests.Home exposing(..)

import Msgs.Home exposing (Msg)
import HttpBuilder as HB

import Decoder exposing (decodeTopics)

getTopics : Cmd Msg
getTopics =
    HB.get "/topics"
    |> HB.withExpectJson decodeTopics
    |> HB.send ReceiveTopics
