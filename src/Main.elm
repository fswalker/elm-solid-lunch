port module Main exposing (main)

import Browser
import Debug
import Html exposing (Html, button, dl, dt, dd, div, input, label, p, span, text)
import Html.Attributes exposing (for, id, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Ports

type alias WebId = String

type alias FullName = String

type alias Data =
    { webId : WebId
    , profileInput : String
    , fullName : FullName
    }

type Model 
    = Anonymous
    | LoggedIn Data

type Msg
    = NoOp
    | AuthRequest
    | LogIn WebId
    | LogOut
    | FetchProfile
    | LoadProfile FullName
    | UpdateProfileInput String

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
        LoggedIn data ->
            div [] 
                [ p [] [ text "You are logged in as:" ]
                , p [] [ span [] [ text data.webId ] ]
                , button [ onClick LogOut ] [ text "Log out" ]
                , p []
                    [ label [ for "profile" ] [ text "Profile: " ]
                    , input 
                        [ id "profile"
                        , type_ "text"
                        , style "width" "25em"
                        , value (if data.profileInput == "" then data.webId else data.profileInput)
                        , onInput UpdateProfileInput
                        ] [ ]
                    , button [ onClick FetchProfile ] [ text "View" ]
                    ]
                , dl []
                    [ dt [] [ text "Full name" ]
                    , dd [] [ text data.fullName ]
                    ]
                ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        NoOp ->
            let 
                _ = 
                    Debug.log "model" model
            in
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
        LogIn webId ->
            if model == Anonymous then
                (LoggedIn (Data webId webId ""), Cmd.none)
            else
                (model, Cmd.none)
        FetchProfile ->
            case model of
                LoggedIn data ->
                    (model, Ports.fetchProfile data.profileInput)
                _ -> 
                    (model, Cmd.none)
        LoadProfile fullName ->
            (setModelFullName model fullName, Cmd.none)
        UpdateProfileInput profile ->
            (setModelProfileInput model profile, Cmd.none)


setModelFullName : Model -> String -> Model
setModelFullName model fullName =
    case model of
        Anonymous -> model
        LoggedIn data ->
            LoggedIn { data | fullName = fullName }

setModelProfileInput : Model -> String -> Model
setModelProfileInput model profile =
    case model of
        Anonymous -> model
        LoggedIn data ->
            LoggedIn { data | profileInput = profile }

sessionSubscriptionHandler : D.Value -> Msg
sessionSubscriptionHandler =
    D.decodeValue D.string
    >> Result.map LogIn
    >> Result.withDefault LogOut

loadProfileHandler : D.Value -> Msg
loadProfileHandler =
    D.decodeValue D.string
    >> Result.map LoadProfile
    >> Result.withDefault NoOp

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
    [ Ports.trackSession sessionSubscriptionHandler
    , Ports.loadProfile loadProfileHandler
    ]

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
