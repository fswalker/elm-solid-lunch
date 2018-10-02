port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, p, span, text)
import Html.Events exposing (onClick)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Ports

type Model 
    = Anonymous
    | LoggedIn String

type alias WebId = String

type Msg
    = NoOp
    | AuthRequest
    | LogIn WebId
    | LogOut

init : () -> (Model, Cmd Msg)
init flags = (Anonymous, Cmd.none)

view : Model -> Html Msg
view model = 
    case model of
        Anonymous ->
            div [] 
                [ p [] [ text "You are not logged in." ]
                , button [ onClick AuthRequest ] [ text "Log in" ]
                ]
        LoggedIn user ->
            div [] 
                [ p [] [ text "You are logged in as:" ]
                , p [] [ span [] [ text user ] ]
                , button [ onClick LogOut ] [ text "Log out" ]
                ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        NoOp ->
            (model, Cmd.none)
        AuthRequest ->
            if model == Anonymous then
                (model, Ports.authRequest ())
            else
                (model, Cmd.none)
        LogOut ->
            if model /= Anonymous then
                (Anonymous, Ports.logout ())
            else
                (model, Cmd.none)
        LogIn user ->
            if model == Anonymous then
                (LoggedIn user, Cmd.none)
            else
                (model, Cmd.none)

sessionSubscriptionHandler : D.Value -> Msg
sessionSubscriptionHandler =
    D.decodeValue D.string
    >> Result.map LogIn
    >> Result.withDefault LogOut

subscriptions : Model -> Sub Msg
subscriptions model =
    Ports.trackSession sessionSubscriptionHandler

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
