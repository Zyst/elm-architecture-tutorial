module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { firstDieFace : Int
    , secondDieFace : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 1 1
    , Cmd.none
    )



-- UPDATE


type Msg
    = Roll
    | NewFace ( Int, Int )


generatePair msg =
    Random.generate msg
        (Random.pair (Random.int 1 6) (Random.int 1 6))



-- TODO: Have the dice flip around randomly before they settle on a final value.
-- Do this once you understand Elm a bit better
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model, generatePair NewFace )

        NewFace ( firstDice, secondDice ) ->
            ( Model firstDice secondDice
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


createDice : Int -> List (Svg msg)
createDice face =
    let
        rectangle =
            rect
                -- This here is what I want to append it after
                [ x "10"
                , y "10"
                , width "130"
                , height "130"
                , rx "20"
                , ry "20"
                , fill "white"
                , stroke "black"
                ]
                []

        center =
            circle
                [ cx "75"
                , cy "75"
                , r "10"
                , fill "black"
                ]
                []

        topLeft =
            circle
                [ cx "45"
                , cy "45"
                , r "10"
                , fill "black"
                ]
                []

        topRight =
            circle
                [ cx "105"
                , cy "45"
                , r "10"
                , fill "black"
                ]
                []

        bottomLeft =
            circle
                [ cx "45"
                , cy "105"
                , r "10"
                , fill "black"
                ]
                []

        bottomRight =
            circle
                [ cx "105"
                , cy "105"
                , r "10"
                , fill "black"
                ]
                []
    in
    case face of
        1 ->
            [ rectangle, center ]

        2 ->
            [ rectangle
            , topLeft
            , bottomRight
            ]

        3 ->
            [ rectangle
            , topLeft
            , center
            , bottomRight
            ]

        4 ->
            [ rectangle
            , topLeft
            , topRight
            , bottomLeft
            , bottomRight
            ]

        5 ->
            [ rectangle
            , topLeft
            , topRight
            , center
            , bottomLeft
            , bottomRight
            ]

        6 ->
            [ rectangle
            , topLeft
            , topRight
            , bottomLeft
            , bottomRight
            , circle
                [ cx "45"
                , cy "75"
                , r "10"
                , fill "black"
                ]
                []
            , circle
                [ cx "105"
                , cy "75"
                , r "10"
                , fill "black"
                ]
                []
            ]

        _ ->
            [ rectangle
            , circle
                [ r "0"
                ]
                []
            ]


view : Model -> Html Msg
view { firstDieFace, secondDieFace } =
    div []
        [ svg
            [ width "150"
            , height "150"
            , viewBox "0 0 150 150"
            ]
            (createDice firstDieFace)
        , svg
            [ width "150"
            , height "150"
            , viewBox "0 0 150 150"
            ]
            (createDice secondDieFace)
        , button [ onClick Roll ] [ Html.text "Roll" ]
        ]
