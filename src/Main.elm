port module Main exposing (main)

import Browser
import Debug
import Html exposing (Html, a, button, dl, dt, dd, div, input, label, li, p, span, text, ul)
import Html.Attributes exposing (for, href, id, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Ports

type alias WebId = String

type alias FullName = String

type alias FriendData =
    { name  : FullName
    , webId : WebId
    }

type alias Profile =
    { fullName : FullName
    , friends  : List FriendData
    }

type alias Data =
    { webId : WebId
    , profileInput : String
    , profile : Maybe Profile
    }

type Model 
    = Anonymous
    | LoggedIn Data

type Msg
    = NoOp
    | AuthRequest
    | LogIn WebId
    | LogOut
    | FetchProfile WebId
    | LoadProfile Profile
    | UpdateProfileInput String
    | ChooseFriend WebId

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
                    , button [ onClick <| FetchProfile data.profileInput ] [ text "View" ]
                    ]
                , dl []
                    [ dt [] [ text "Full name" ]
                    , dd [] [ text <| Maybe.withDefault "" <| Maybe.map .fullName data.profile ]
                    , dt [] [ text "Friends" ]
                    , dd []
                        [ case data.profile of
                            Just p ->
                                ul [] <| List.map displayFriend p.friends
                            _ -> 
                                text ""
                        ]
                    ]
                ]

displayFriend : FriendData -> Html Msg
displayFriend friend =
    li [] 
        [ a 
            [ href "#"
            , onClick (ChooseFriend friend.webId) 
            ] 
            [ text <|
                if friend.name /= "" then 
                    friend.name 
                else
                    friend.webId
            ] 
        ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        NoOp ->
            let 
                _ = 
                    Debug.log "msg: NoOp, model:" model
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
                (LoggedIn (Data webId webId Nothing), Ports.fetchProfile webId)
            else
                (model, Cmd.none)
        FetchProfile webId ->
            case model of
                LoggedIn data ->
                    (LoggedIn { data | profile = Nothing }, Ports.fetchProfile webId)
                _ -> 
                    (model, Cmd.none)
        LoadProfile data ->
            (setProfile model data, Cmd.none)
        UpdateProfileInput profile ->
            (setModelProfileInput model profile, Cmd.none)
        ChooseFriend webId ->
            case model of
                LoggedIn data ->
                    (LoggedIn 
                        { data 
                        | profileInput = webId
                        , profile = Nothing
                        }
                        , Ports.fetchProfile webId)
                _ -> 
                    (model, Cmd.none)



setProfile : Model -> Profile -> Model
setProfile model profile =
    case model of
        Anonymous -> model
        LoggedIn data ->
            LoggedIn { data | profile = Just profile }

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

profileDecoder : Decoder Profile
profileDecoder =
    let
        friendDecoder =
            D.map2 FriendData
                (D.field "name" D.string)
                (D.field "webId" D.string)
    in
        D.map2 Profile
            (D.field "fullName" D.string)
            (D.field "friends" (D.list friendDecoder))

loadProfileHandler : D.Value -> Msg
loadProfileHandler =
    D.decodeValue profileDecoder
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
