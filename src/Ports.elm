port module Ports exposing (..)

import Json.Decode as D

port authRequest : () -> Cmd msg

port logout : () -> Cmd msg

port trackSession : (D.Value -> msg) -> Sub msg
