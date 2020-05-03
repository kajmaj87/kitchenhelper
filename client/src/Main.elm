module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http exposing (..)
import Json.Decode as D exposing (decodeString, list, string)
import Json.Decode.Pipeline as P exposing (required)
import Set exposing (Set)



-- MAIN


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MODEL


type alias Dish =
    { name : String
    , tags : List String
    , desc : String
    }


type alias Model =
    { tagsToSearch : String
    , dishes : List Dish
    , status : String
    }


dummyDishes =
    [ { name = "Pomidorowka", tags = [ "zupa", "proste" ], desc = "Bla bla" }, { name = "Tortilla", tags = [ "szybkie", "proste" ], desc = "Bla tortilla bla" } ]


init : () -> ( Model, Cmd Msg )
init () =
    ( { tagsToSearch = ""
      , dishes = dummyDishes
      , status = ""
      }
    , Http.get
        { url = "http://192.168.1.231:8080/dishes.json"
        , expect = Http.expectJson GotJson (D.list dishDecoder)
        }
    )


dishDecoder : D.Decoder Dish
dishDecoder =
    D.succeed Dish
        |> P.required "name" D.string
        |> P.required "tags" (D.list D.string)
        |> P.required "desc" D.string



-- UPDATE


type Msg
    = ChangeTags String
    | GotJson (Result Http.Error (List Dish))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeTags newTags ->
            ( { model | tagsToSearch = newTags }, Cmd.none )

        GotJson result ->
            case result of
                Ok newDishes ->
                    ( { model | dishes = newDishes }, Cmd.none )

                Err _ ->
                    ( { model | status = "Error loading data" }, Cmd.none )


filterDishes : List String -> List Dish -> List Dish
filterDishes tags dishes =
    List.filter (\dish -> List.all (\tag -> List.member tag (.tags dish)) tags) dishes


tagsToList : String -> List String
tagsToList tags =
    List.filter (\e -> not (String.isEmpty e)) (List.map String.trim (String.split " " tags))


possibleTags : List Dish -> Set String
possibleTags dishes =
    Set.fromList (List.concat (List.map .tags dishes))


relevantDishes : Model -> List Dish
relevantDishes model =
    filterDishes (tagsToList model.tagsToSearch) model.dishes



-- VIEW


viewPossibleTags : List Dish -> Html Msg
viewPossibleTags dishes =
    text ("Possible tags: " ++ String.join ", " (Set.toList (possibleTags dishes)))


viewDishHeader : () -> Html Msg
viewDishHeader () =
    tr []
        [ th [] [ text "Dish" ]
        , th [] [ text "Tags" ]
        , th [] [ text "Desc" ]
        ]


viewDishRow : Dish -> Html Msg
viewDishRow dish =
    tr []
        [ td [] [ text dish.name ]
        , td [] [ text (String.join ", " dish.tags) ]
        , td [] [ text dish.desc ]
        ]


viewDishes : List Dish -> Html Msg
viewDishes dishes =
    table [] (viewDishHeader () :: List.map viewDishRow dishes)


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "Write tags here", value model.tagsToSearch, autofocus True, onInput ChangeTags ] []
        , viewPossibleTags (relevantDishes model)
        , viewDishes (List.sortBy .name (relevantDishes model))
        , text model.status
        ]
