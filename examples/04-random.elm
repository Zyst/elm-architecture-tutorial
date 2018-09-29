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
    { dieFace : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 1
    , Cmd.none
    )



-- UPDATE


type Msg
    = Roll
    | NewFace Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model
            , Random.generate NewFace (Random.int 1 6)
            )

        NewFace newFace ->
            ( Model newFace
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


createCircles : Int -> Svg msg
createCircles face =
    case face of
        1 ->
            circle
                [ cx "75"
                , cy "75"
                , r "10"
                , fill "black"
                ]
                []

        _ ->
            circle
                [ r "0"
                ]
                []


view : Model -> Html Msg
view model =
    div []
        [ svg
            [ width "150"
            , height "150"
            , viewBox "0 0 150 150"
            ]
            [ rect
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
            , createCircles model.dieFace
            ]
        , button [ onClick Roll ] [ Html.text "Roll" ]
        ]
