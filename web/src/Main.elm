module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Item
import Json.Decode as D
import Json.Encode as E
import Random
import Task
import UUID exposing (UUID, generator)



-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = init, subscriptions = subscriptions, update = update, view = view }



-- MODEL


type alias Model =
    { items : List Item.Model
    , entry : String
    , visibility : Visibility
    , httpState : HttpState
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { items = []
      , entry = ""
      , visibility = Active
      , httpState = Loading
      }
    , Cmd.none
    )


type Visibility
    = All
    | Active
    | Completed


type HttpState
    = Loading
    | Failure
    | Success



-- UPDATE


type Msg
    = GenerateUuid
    | AddItem Int
    | EntryChange String
    | ChangeVisibility Visibility
    | UpdateItem Item.Msg UUID
    | Fetch
    | FetchedItems (Result Http.Error (List Item.Model))
    | Save
    | SavedItems (Result Http.Error String)
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateUuid ->
            ( model, Random.generate AddItem (Random.int 100 999) )

        EntryChange entry ->
            ( { model | entry = entry }, Cmd.none )

        AddItem random ->
            let
                uuid =
                    Random.step generator (Random.initialSeed random) |> Tuple.first

                newItem =
                    Item.init model.entry uuid
            in
            ( { model | items = newItem :: model.items, entry = "" }, Cmd.none )

        ChangeVisibility visibility ->
            ( { model | visibility = visibility }, Cmd.none )

        UpdateItem itemMsg uuid ->
            let
                updatedModel =
                    { model
                        | items =
                            List.map
                                (\item ->
                                    if item.uuid == uuid then
                                        Maybe.withDefault item (Item.update itemMsg item)

                                    else
                                        item
                                )
                                model.items
                    }
            in
            case itemMsg of
                Item.Delete ->
                    ( { updatedModel | items = List.filter (\item -> item.uuid /= uuid) model.items }, Cmd.none )

                Item.Focus input_id ->
                    ( updatedModel, Task.attempt (\_ -> NoOp) (Dom.focus input_id) )

                _ ->
                    ( updatedModel, Cmd.none )

        Fetch ->
            ( model
            , Http.get
                { url = "http://plommon.duckdns.org/fetch"
                , expect = Http.expectJson FetchedItems itemsDecoder
                }
            )

        FetchedItems res ->
            case res of
                Ok items ->
                    ( { model | httpState = Success, items = items }, Cmd.none )

                Err _ ->
                    ( { model | httpState = Failure }, Cmd.none )

        Save ->
            ( model
            , Http.post
                { body = Http.jsonBody (itemsEncoder model.items)
                , expect = Http.expectString SavedItems
                , url = "http://plommon.duckdns.org/post"
                }
            )

        SavedItems res ->
            case res of
                Ok _ ->
                    ( { model | httpState = Success }, Cmd.none )

                Err _ ->
                    ( { model | httpState = Failure }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style "width" "50%"
        , style "margin" "auto"
        , style "display" "flex"
        , style "flex-direction" "column"
        ]
        [ buttons
        , inputForm model
        , list model.items model.visibility
        ]


buttons : Html Msg
buttons =
    div
        [ style "width" "100%"
        ]
        [ button [ onClick (ChangeVisibility All) ] [ text "all" ]
        , button [ onClick (ChangeVisibility Active) ] [ text "active" ]
        , button [ onClick (ChangeVisibility Completed) ] [ text "completed" ]
        , button [ onClick Fetch, style "float" "right" ] [ text "fetch" ]
        , button [ onClick Save, style "float" "right" ] [ text "save" ]
        ]


inputForm : Model -> Html Msg
inputForm model =
    div []
        [ Html.form [ onSubmit GenerateUuid ]
            [ input
                [ placeholder "add item"
                , value model.entry
                , onInput EntryChange
                , autofocus True
                , style "width" "100%"
                ]
                []
            ]
        ]


list : List Item.Model -> Visibility -> Html Msg
list items visibility =
    div
        [ style "width" "100%"
        ]
        (List.map
            (\item ->
                let
                    i =
                        item.uuid

                    v =
                        Item.view item
                in
                Html.map (\msg -> UpdateItem msg i) v
            )
            (case visibility of
                All ->
                    items

                Active ->
                    List.filter (\item -> not item.completed) items

                Completed ->
                    List.filter (\item -> item.completed) items
            )
        )



-- HTTP


itemsDecoder : D.Decoder (List Item.Model)
itemsDecoder =
    D.list
        (D.map5 Item.Model
            (D.field "description" D.string)
            (D.field "uuid" UUID.jsonDecoder)
            (D.field "completed" D.bool)
            (D.succeed Nothing)
            (D.succeed Item.Normal)
        )


itemsEncoder : List Item.Model -> E.Value
itemsEncoder items =
    E.list
        (\item ->
            E.object
                [ ( "description", E.string item.description )
                , ( "uuid", UUID.toValue item.uuid )
                , ( "completed", E.bool item.completed )
                ]
        )
        items
