module Example exposing (main)

import Browser
import Dict
import Html exposing (..)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)
import Json.Encode
import Rollbar exposing (Rollbar)
import Task


token : String
token =
    "REPLACE ME WITH YOUR TOKEN"


rollbar : Rollbar
rollbar =
    Rollbar.scoped
        (Rollbar.token token)
        (Rollbar.environment "test")
        "Example"



-- MODEL --


type alias Model =
    { report : String
    }


initialModel : Model
initialModel =
    { report = ""
    }



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


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = \model -> { title = "Example", body = [ view model ] }
        }


init : ( Model, Cmd msg )
init =
    ( initialModel, Cmd.none )
