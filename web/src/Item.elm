module Item exposing (..)

import Html exposing (Attribute, Html , input, label, li, text, div)
import Html.Attributes exposing (..)
import Html.Events exposing (keyCode, on, onClick, onDoubleClick, onInput, onSubmit)
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
    = Completed
    | Delete
    | Focus String
    | EditChange String
    | EditCommit
    | EditCancel
    | NoOp


update : Msg -> Model -> Maybe Model
update msg model =
    case msg of
        Completed ->
            Just { model | completed = True }

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
    li []
        [ case model.visibility of
            Normal ->
                li []
                    [ input [ class "check", type_ "checkbox", checked model.completed, onClick Completed ] []
                    , label [ onDoubleClick (Focus input_id) ] [ text model.description ]
                    , I.x |> I.toHtml [onClick Delete]
                    ]

            Editing ->
                li []
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
