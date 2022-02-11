module Item exposing (..)

import Html exposing (Attribute, Html , input, label, li, text, div)
import Html.Attributes exposing (style, class, type_, checked, id, value)
import Html.Events exposing (keyCode, on, onClick, onDoubleClick, onInput)
import Json.Decode as D
import UUID exposing (UUID)
import FeatherIcons as I



-- import Html exposing (..)
-- import Html.Attributes exposing (..)
-- import Html.Events exposing (..)
-- import Json.Decode
-- MODEL


type alias Model =
    { description : String
    , uuid : UUID
    , completed : Bool
    , edit : Maybe String
    , visibility : Visibility
    }


init : String -> UUID -> Model
init description uuid =
    { description = description
    , uuid = uuid
    , completed = False
    , edit = Nothing
    , visibility = Normal
    }


type Visibility
    = Normal
    | Editing



-- UPDATE


type Msg
    = Toggle
    | Delete
    | Focus String
    | EditChange String
    | EditCommit
    | EditCancel
    | NoOp


update : Msg -> Model -> Maybe Model
update msg model =
    case msg of
        Toggle ->
            Just { model | completed = not model.completed }

        EditChange edit ->
            Just { model | edit = Just edit }

        EditCommit ->
            Just { model | description = Maybe.withDefault model.description model.edit, edit = Nothing, visibility = Normal }

        EditCancel ->
            Just { model | edit = Nothing, visibility = Normal }

        Focus _ ->
            Just { model | visibility = Editing }

        _ ->
            Nothing



-- View


view : Model -> Html Msg
view model =
    let
        input_id =
            "input_" ++ UUID.toString model.uuid
    in
    li [ style "list-style-type" "none"]
        [ case model.visibility of
            Normal ->
                div []
                    [ input [ class "check", type_ "checkbox", checked model.completed, onClick Toggle ] []
                    , label [ onDoubleClick (Focus input_id) ] [ text model.description ]
                    , I.x |> I.withSize 16 |> I.toHtml [onClick Delete, style "float" "right"]
        
                    ]

            Editing ->
                div []
                    [ input 
                    [  class "edit", id input_id
                    , onInput EditChange, onFinish EditCommit EditCancel NoOp
                    , value (Maybe.withDefault model.description model.edit) ] []
                    ]
        ]


onFinish : msg -> msg -> msg -> Attribute msg
onFinish enter esc noOp =
    let
        select key =
            case key of
                13 ->
                    enter

                27 ->
                    esc

                _ ->
                    noOp
    in
    on "keydown" (D.map select keyCode)
