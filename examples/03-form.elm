module Main exposing (Model, Msg(..), init, main, update, validate, view, viewInput, viewValidation)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { name : String
    , age : Int
    , password : String
    , passwordAgain : String
    , submitted : Bool
    }


init : Model
init =
    Model "" 0 "" "" False



-- UPDATE


type Msg
    = Name String
    | Age String
    | Password String
    | PasswordAgain String
    | Submit


update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name ->
            { model | name = name, submitted = False }

        Password password ->
            { model | password = password, submitted = False }

        PasswordAgain password ->
            { model | passwordAgain = password, submitted = False }

        Age age ->
            { model | age = Maybe.withDefault 0 (String.toInt age), submitted = False }

        Submit ->
            { model | submitted = True }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewInput "text" "Name" model.name Name
        , viewInput "number" "Age" (String.fromInt model.age) Age
        , viewInput "password" "Password" model.password Password
        , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
        , button [ onClick Submit ] [ text "Submit" ]
        , viewValidation model
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


validate : Model -> Bool
validate { password, passwordAgain, age } =
    -- Passwords match
    password
        == passwordAgain
        -- Password length is > 8
        && String.length password
        > 8
        -- Password contains lowercase letters
        && String.toUpper password
        /= password
        -- Password contains uppercase letters
        && String.toLower password
        /= password
        -- Password contains a number
        && String.any Char.isDigit password


viewValidation : Model -> Html msg
viewValidation model =
    if not model.submitted then
        div [] []

    else if validate model then
        div [ style "color" "green" ] [ text "OK" ]

    else
        div [ style "color" "red" ] [ text "Passwords must match, be longer than 8 characters, must contain upper case, lower case, AND numeric characters. Age must be a number." ]
