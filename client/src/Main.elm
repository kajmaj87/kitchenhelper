port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (attribute, autofocus, class, for, href, id, placeholder, tabindex, target, title, type_, value)
import Html.Events exposing (onClick, onInput)
import Http exposing (..)
import Json.Decode as D exposing (decodeString, list, string)
import Json.Decode.Pipeline as P exposing (required)
import Json.Encode as E exposing (object)
import Set exposing (Set)



-- MAIN


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- PORTS


port sendMessage : String -> Cmd msg



-- MODEL


type alias Dish =
    { id : Maybe Int
    , name : String
    , tags : List String
    , desc : String
    , link : String
    }


type alias Model =
    { tagsToSearch : String
    , dishes : List Dish
    , currentlyEdited : Maybe Dish
    , status : String
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { tagsToSearch = ""
      , dishes = []
      , currentlyEdited = Nothing
      , status = ""
      }
    , Http.get
        { url = "/dishes"
        , expect = Http.expectJson GotDishesListJson (D.list dishDecoder)
        }
    )


saveDish : Dish -> Cmd Msg
saveDish dish =
    case dish.id of
        -- this is the update case
        Just id ->
            Http.request
                { method = "PUT"
                , headers = []
                , url = "/dishes/" ++ String.fromInt id
                , body = Http.jsonBody (dishEncoder dish)
                , expect = Http.expectJson GotSingleDishJson dishDecoder
                , timeout = Nothing
                , tracker = Nothing
                }

        Nothing ->
            Cmd.none


idEncoder : Maybe Int -> E.Value
idEncoder maybeId =
    case maybeId of
        Just id ->
            E.int id

        Nothing ->
            E.null


dishEncoder : Dish -> E.Value
dishEncoder dish =
    E.object
        [ ( "id", idEncoder dish.id )
        , ( "name", E.string dish.name )
        , ( "tags", E.list E.string dish.tags )
        , ( "desc", E.string dish.desc )
        , ( "link", E.string dish.link )
        ]


dishDecoder : D.Decoder Dish
dishDecoder =
    D.succeed Dish
        |> P.required "id" (D.maybe D.int)
        |> P.required "name" D.string
        |> P.required "tags" (D.list D.string)
        |> P.optional "desc" D.string "-"
        |> P.optional "link" D.string ""



-- UPDATE


type Msg
    = ChangeSearchTags String
    | ChangeName Dish String
    | ChangeTags Dish String
    | ChangeDesc Dish String
    | ChangeLink Dish String
    | StartEditing Dish
    | SaveDish Dish
    | SaveToStatusField String
    | GotSingleDishJson (Result Http.Error Dish)
    | GotDishesListJson (Result Http.Error (List Dish))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeSearchTags newTags ->
            ( { model | tagsToSearch = newTags }, Cmd.none )

        ChangeName dish new ->
            ( { model | currentlyEdited = Just { dish | name = Debug.log "Setting new name to: " new } }, Cmd.none )

        ChangeTags dish new ->
            ( { model | currentlyEdited = Just { dish | tags = Debug.log "Setting tags name to: " (tagsToList new) } }, Cmd.none )

        ChangeDesc dish new ->
            ( { model | currentlyEdited = Just { dish | desc = Debug.log "Setting desc name to: " new } }, Cmd.none )

        ChangeLink dish new ->
            ( { model | currentlyEdited = Just { dish | link = Debug.log "Setting link name to: " new } }, Cmd.none )

        StartEditing dish ->
            ( { model | currentlyEdited = Just dish }, sendMessage "openModal" )

        SaveDish dish ->
            ( { model | currentlyEdited = Nothing }, Cmd.batch [ sendMessage "closeModal", saveDish dish ] )

        SaveToStatusField value ->
            ( { model | status = Debug.log "Status: " value }, Cmd.none )

        GotSingleDishJson result ->
            case result of
                Ok dish ->
                    ( { model | dishes = List.map (replaceWhenIdMatches dish.id dish) model.dishes }, Cmd.none )

                Err error ->
                    ( { model | status = "Error loading data " ++ Debug.toString error }, Cmd.none )

        GotDishesListJson result ->
            case result of
                Ok newDishes ->
                    ( { model | dishes = List.map tagsToLowercase newDishes }, Cmd.none )

                Err error ->
                    ( { model | status = "Error loading data " ++ Debug.toString error }, Cmd.none )


replaceWhenIdMatches : Maybe Int -> Dish -> Dish -> Dish
replaceWhenIdMatches maybeId newDish oldDish =
    case maybeId of
        Just id ->
            case oldDish.id of
                Just oldId ->
                    if oldId == id then
                        newDish

                    else
                        oldDish

                Nothing ->
                    oldDish

        Nothing ->
            oldDish


filterDishes : List String -> List Dish -> List Dish
filterDishes tags dishes =
    List.filter (\dish -> List.all (\tag -> List.member tag (.tags dish)) tags) dishes


tagsToList : String -> List String
tagsToList tags =
    List.filter (\e -> not (String.isEmpty e)) (List.map String.trim (String.split " " tags))


tagsToString : List String -> String
tagsToString tags =
    String.join " " (List.sort tags)


tagsToLowercase : Dish -> Dish
tagsToLowercase dish =
    { dish | tags = List.map String.toLower dish.tags }


uniqueTags : List Dish -> Set String
uniqueTags dishes =
    Set.fromList (List.concat (List.map .tags dishes))


relevantDishes : Model -> List Dish
relevantDishes model =
    filterDishes (tagsToList model.tagsToSearch) model.dishes


dishOrDefualt : Maybe Dish -> Dish
dishOrDefualt dish =
    case dish of
        Just d ->
            d

        Nothing ->
            Dish Nothing "Empty dish" [] "No description" ""



-- VIEW


viewDishHeader : () -> Html Msg
viewDishHeader () =
    thead []
        [ tr []
            [ th [] [ text "Dish" ]
            , th [] [ text "Desc" ]
            ]
        ]


viewDishNameAsLink : Dish -> Html Msg
viewDishNameAsLink dish =
    if dish.link == "" then
        text dish.name

    else
        a [ href dish.link, target "_blank" ] [ text dish.name ]


viewDishRow : Dish -> Html Msg
viewDishRow dish =
    tr []
        [ td [] [ viewDishNameAsLink dish ]
        , td [ onClick (StartEditing dish) ] [ text dish.desc ]
        ]


viewDishes : List Dish -> Html Msg
viewDishes dishes =
    table [ class "table table-hover table-responsive table-striped" ] [ viewDishHeader (), tbody [] (List.map viewDishRow dishes) ]


viewButtonWithPopover : String -> String -> List (Html Msg) -> Html Msg
viewButtonWithPopover popupTitle popupText buttonContents =
    button
        [ type_ "btn"
        , class "btn btn-primary"
        , title popupTitle
        , attribute "data-toggle" "popover"
        , attribute "data-trigger" "focus"
        , attribute "data-content" popupText
        ]
        buttonContents


viewTags : Model -> List String -> List (Html Msg)
viewTags model tags =
    [ div [ class "col-auto" ]
        [ input
            [ class "form-control"
            , placeholder "Write tags here"
            , value model.tagsToSearch
            , autofocus True
            , onInput ChangeSearchTags
            ]
            []
        ]
    , div [ class "col-auto" ]
        [ viewButtonWithPopover
            "You can choose from: "
            (tagsToString tags)
            [ text "Tags ", span [ class "badge badge-light" ] [ text (String.fromInt (List.length tags)) ] ]
        ]
    ]


viewTopBar : Model -> Html Msg
viewTopBar model =
    div [ class "form-row align-items-center m-3" ]
        (viewTags model (Set.toList (uniqueTags (relevantDishes model))))


viewEditDialogForm : Dish -> Html Msg
viewEditDialogForm dish =
    form []
        [ div [ class "form-group" ]
            [ label [ for "dish-name", class "col-form-label" ] [ text "Name: " ]
            , input [ id "dish-name", type_ "text", class "form-control", autofocus True, value dish.name, onInput (ChangeName dish) ] []
            ]
        , div
            [ class "form-group" ]
            [ label [ for "dish-tags", class "col-form-label" ] [ text "Tags: " ]
            , textarea [ id "dish-tags", class "form-control", value (tagsToString dish.tags), onInput (ChangeTags dish) ] []
            ]
        , div
            [ class "form-group" ]
            [ label [ for "dish-link", class "col-form-label" ] [ text "Link: " ]
            , textarea [ id "dish-link", class "form-control", value dish.link, onInput (ChangeLink dish) ] []
            ]
        , div
            [ class "form-group" ]
            [ label [ for "dish-desc", class "col-form-label" ] [ text "Desc: " ]
            , textarea [ id "dish-desc", class "form-control", value dish.desc, onInput (ChangeDesc dish) ] []
            ]
        ]


viewEditDialogFooter : Dish -> Html Msg
viewEditDialogFooter dish =
    div [ class "modal-footer" ]
        [ button [ class "btn btn-secondary", attribute "data-dismiss" "modal" ] [ text "Close" ]
        , button [ class "btn btn-primary", onClick (SaveDish dish) ] [ text "Save" ]
        ]


viewEditDialog : Dish -> Html Msg
viewEditDialog dish =
    div [ class "modal fade", id "modal", tabindex -1, attribute "role" "dialog" ]
        [ div [ class "modal-dialog", attribute "role" "document" ]
            [ div [ class "modal-content" ]
                [ div [ class "modal-body" ] [ viewEditDialogForm dish ]
                , viewEditDialogFooter dish
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ viewEditDialog (dishOrDefualt model.currentlyEdited)
        , viewTopBar model
        , viewDishes (List.sortBy .name (relevantDishes model))
        , text model.status
        ]
