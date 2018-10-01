module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Svg exposing (circle, line, svg)
import Svg.Attributes exposing (cx, cy, fill, height, r, stroke, strokeWidth, viewBox, width, x1, x2, y1, y2)
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


type alias Coordinates =
    { x : Float
    , y : Float
    }


rotateClockwiseByDegrees : Float -> Coordinates -> Coordinates
rotateClockwiseByDegrees d { x, y } =
    Coordinates
        ((x * cos (degrees d)) - (y * sin (degrees d)))
        ((x * sin (degrees d)) + (y * cos (degrees d)))


createSecondsCoordinates : Model -> Coordinates
createSecondsCoordinates { zone, time } =
    let
        pointingUp =
            Coordinates 0 -55

        secondsInDegrees =
            toFloat (Time.toSecond zone time * 6)
    in
    rotateClockwiseByDegrees secondsInDegrees pointingUp


createMinutesCoordinates : Model -> Coordinates
createMinutesCoordinates { zone, time } =
    let
        pointingUp =
            Coordinates 0 -60

        minutesInDegrees =
            toFloat (Time.toMinute zone time * 6)
    in
    rotateClockwiseByDegrees minutesInDegrees pointingUp


createHourCoordinates : Model -> Coordinates
createHourCoordinates { zone, time } =
    let
        pointingUp =
            Coordinates 0 -35

        hoursInDegrees =
            toFloat (modBy 12 (Time.toHour zone time) * 30)
    in
    rotateClockwiseByDegrees hoursInDegrees pointingUp


createClock model =
    let
        secondsHand =
            createSecondsCoordinates model

        minutesHand =
            createMinutesCoordinates model

        hoursHand =
            createHourCoordinates model
    in
    svg
        [ width "150"
        , height "150"
        , viewBox "-75 -75 150 150"
        ]
        [ circle
            [ cx "0"
            , cy "0"
            , width "130"
            , height "130"
            , fill "white"
            , stroke "black"
            , r "65"
            ]
            []
        , circle
            [ cx "0"
            , cy "0"
            , r "1.5"
            ]
            []

        -- Hours
        , line
            [ x1 "0"
            , y1 "0"
            , x2 (String.fromFloat hoursHand.x)
            , y2 (String.fromFloat hoursHand.y)
            , stroke "black"
            , strokeWidth "1.5"
            ]
            []

        -- Minutes
        , line
            [ x1 "0"
            , y1 "0"
            , x2 (String.fromFloat minutesHand.x)
            , y2 (String.fromFloat minutesHand.y)
            , stroke "black"
            , strokeWidth "1.5"
            ]
            []

        -- Seconds
        , line
            [ x1 "0"
            , y1 "0"
            , x2 (String.fromFloat secondsHand.x)
            , y2 (String.fromFloat secondsHand.y)
            , stroke "red"
            ]
            []
        ]


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
        , createClock model
        , button
            [ onClick TogglePause ]
            [ text buttonText ]
        ]
