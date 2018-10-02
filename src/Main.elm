module Main exposing (main)

import Browser
import Html exposing (Html, div)


init : () -> ( (), Cmd msg)
init flags = ((), Cmd.none)

view : () -> Html msg
view model = 
    div [] []

update : () -> msg -> ((), Cmd msg)
update model msg = (model, Cmd.none)

subscriptions : () -> Sub msg
subscriptions model =
    Sub.none

main : Program () () ()
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
