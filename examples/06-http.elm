module Main exposing (Model, Msg(..), getRandomGif, gifDecoder, init, main, subscriptions, toGiphyUrl, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Url.Builder as Url



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
    { topic : String
    , url : String
    , error : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "dog" "waiting.gif" "Loading"
    , getRandomGif "dog"
    )



-- UPDATE


type Msg
    = MorePlease
    | NewGif (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( model
            , getRandomGif model.topic
            )

        NewGif result ->
            case result of
                Ok newUrl ->
                    ( { model | url = newUrl, error = "" }
                    , Cmd.none
                    )

                Err err ->
                    case err of
                        Http.BadUrl _ ->
                            ( { model
                                | error = "We did not provide a valid URL"
                              }
                            , Cmd.none
                            )

                        Http.Timeout ->
                            ( { model
                                | error = "The server took too long to respond"
                              }
                            , Cmd.none
                            )

                        Http.NetworkError ->
                            ( { model
                                | error = "Network error ocurred, please try again"
                              }
                            , Cmd.none
                            )

                        Http.BadStatus _ ->
                            ( { model
                                | error = "Server responded an error"
                              }
                            , Cmd.none
                            )

                        Http.BadPayload _ _ ->
                            ( { model
                                | error = "Server responded with a bad payload"
                              }
                            , Cmd.none
                            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text model.topic ]
        , p [] [ text model.error ]
        , img [ src model.url ] []
        , hr [] []
        , button [ onClick MorePlease ] [ text "More Please!" ]
        ]



-- HTTP


getRandomGif : String -> Cmd Msg
getRandomGif topic =
    Http.send NewGif (Http.get (toGiphyUrl topic) gifDecoder)


toGiphyUrl : String -> String
toGiphyUrl topic =
    Url.crossOrigin "https://api.giphy.com"
        [ "v1", "gifs", "random" ]
        [ Url.string "api_key" "dc6zaTOxFJmzC"
        , Url.string "tag" topic
        ]


gifDecoder : Decode.Decoder String
gifDecoder =
    Decode.field "data" (Decode.field "image_url" Decode.string)
