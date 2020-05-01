module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    String


init : Model
init =
    ""



-- UPDATE


type Msg
    = Change String


dropLast : List a -> List a
dropLast list =
    List.take (List.length list - 1) list


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newContent ->
            newContent



-- VIEW


stringifyList : List Int -> String
stringifyList list =
    String.join ", " (List.map String.fromInt list)


listifyString : String -> List Int
listifyString string =
    List.filterMap String.toInt (String.split "," string)


hasValue : Maybe a -> Bool
hasValue m =
    case m of
        Just _ ->
            True

        Nothing ->
            False


purify : String -> String
purify s =
    purifyMap identity s


purifyMap : (Int -> Int) -> String -> String
purifyMap map s =
    stringifyList (List.map map (listifyString s))


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "Write your list here", value model, onInput Change ] []
        , div [] [ text (purify model) ]
        , div [] [ text (purifyMap (\a -> a * 2) model) ]
        , div [] [ text (purifyMap (\a -> a ^ 2) model) ]
        ]
