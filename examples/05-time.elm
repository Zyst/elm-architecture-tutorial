module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Task
import Time



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { zone : Time.Zone
    , time : Time.Posix
    , paused : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Time.utc (Time.millisToPosix 0) False
    , Task.perform AdjustTimeZone Time.here
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | TogglePause


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = newTime }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )

        TogglePause ->
            ( { model | paused = not model.paused }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.paused then
        Sub.none

    else
        Time.every 1000 Tick



-- VIEW


zeroPad : String -> Int -> String
zeroPad string goal =
    let
        length =
            String.length string
    in
    if length >= goal then
        string

    else
        String.repeat (goal - length) "0" ++ string


view : Model -> Html Msg
view model =
    let
        hour =
            zeroPad (String.fromInt (Time.toHour model.zone model.time)) 2

        minute =
            zeroPad (String.fromInt (Time.toMinute model.zone model.time)) 2

        second =
            zeroPad (String.fromInt (Time.toSecond model.zone model.time)) 2

        buttonText =
            if model.paused then
                "Resume"

            else
                "Pause"
    in
    div [ style "font-family" "Helvetica" ]
        [ h1
            [ style "color" "#7898b8"
            , style "background-color" "#3d4452"
            , style "padding" "20px"
            ]
            [ text (hour ++ ":" ++ minute ++ ":" ++ second) ]
        , button
            [ onClick TogglePause ]
            [ text buttonText ]
        ]
