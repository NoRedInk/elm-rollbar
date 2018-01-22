module Example exposing (main)

import Dict
import Html exposing (..)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)
import Json.Encode
import Rollbar exposing (Rollbar)
import Task


rollbar : Rollbar
rollbar =
    Rollbar.scoped
        (Rollbar.token "12c99de67a444c229fca100e0967486f")
        (Rollbar.environment "test")
        "Example"



-- MODEL --


type alias Model =
    { report : String }


initialModel : Model
initialModel =
    { report = "" }



-- UPDATE --


type Msg
    = SetText String
    | NoOp
    | Send


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetText text ->
            ( { model | report = text }, Cmd.none )

        Send ->
            ( model, info model.report )


info : String -> Cmd Msg
info report =
    Task.attempt (\_ -> NoOp) (rollbar.info report Dict.empty)


json : Json.Encode.Value
json =
    Json.Encode.object [ ( "environment", Json.Encode.string "test" ) ]



-- VIEW --


view : Model -> Html Msg
view model =
    div []
        [ input [ onInput SetText, value model.report ] []
        , button [ onClick Send ] [ text "Send to rollbar" ]
        ]



-- INIT --


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }


init : ( Model, Cmd msg )
init =
    ( initialModel, Cmd.none )
