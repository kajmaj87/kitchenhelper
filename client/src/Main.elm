module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Set exposing (Set)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Dish =
    { name : String
    , tags : List String
    , desc : String
    }


type alias Model =
    { tagsToSearch : String
    , dishes : List Dish
    }


dummyDishes =
    [ { name = "Pomidorowka", tags = [ "zupa", "proste" ], desc = "Bla bla" }, { name = "Tortilla", tags = [ "szybkie", "proste" ], desc = "Bla tortilla bla" } ]


init : Model
init =
    { tagsToSearch = ""
    , dishes = dummyDishes
    }



-- UPDATE


type Msg
    = ChangeTags String


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeTags newTags ->
            { model | tagsToSearch = newTags }


filterDishes : List String -> List Dish -> List Dish
filterDishes tags dishes =
    List.filter (\dish -> List.all (\tag -> List.member tag (.tags dish)) tags) dishes


tagsToList : String -> List String
tagsToList tags =
    List.filter (\e -> not (String.isEmpty e)) (List.map String.trim (String.split "," tags))


possibleTags : List Dish -> Set String
possibleTags dishes =
    Set.fromList (List.concat (List.map .tags dishes))



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


viewDishRows : List Dish -> List (Html Msg)
viewDishRows dishes =
    List.map
        (\dish ->
            tr []
                [ td [] [ text dish.name ]
                , td [] [ text (String.join ", " dish.tags) ]
                , td [] [ text dish.desc ]
                ]
        )
        dishes


viewDishes : List Dish -> Html Msg
viewDishes dishes =
    table []
        (List.concat
            [ List.singleton (viewDishHeader ()), viewDishRows dishes ]
        )


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "Write tags here", value model.tagsToSearch, autofocus True, onInput ChangeTags ] []
        , viewPossibleTags model.dishes
        , viewDishes (filterDishes (tagsToList model.tagsToSearch) model.dishes)
        ]
