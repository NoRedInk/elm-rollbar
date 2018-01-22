module Rollbar exposing (Environment, Level(..), Rollbar, Scope, Token, environment, scope, scoped, send, token)

{-| Send reports to Rollbar.
-}

import Dict exposing (Dict)
import Http
import Json.Encode exposing (Value)
import Random.Pcg as Random
import Rollbar.Internal exposing (version)
import Task exposing (Task)
import Time exposing (Time)
import Uuid exposing (Uuid, uuidGenerator)


type alias Rollbar =
    { critical : String -> Dict String Value -> Task Http.Error Uuid
    , error : String -> Dict String Value -> Task Http.Error Uuid
    , warning : String -> Dict String Value -> Task Http.Error Uuid
    , info : String -> Dict String Value -> Task Http.Error Uuid
    , debug : String -> Dict String Value -> Task Http.Error Uuid
    }


type Level
    = Critical
    | Error
    | Warning
    | Info
    | Debug


type Token
    = Token String


token : String -> Token
token =
    Token


type Scope
    = Scope String


scope : String -> Scope
scope =
    Scope


type Environment
    = Environment String


environment : String -> Environment
environment =
    Environment


levelToString : Level -> String
levelToString report =
    case report of
        Critical ->
            "critical"

        Debug ->
            "debug"

        Error ->
            "error"

        Info ->
            "info"

        Warning ->
            "warning"


endpointUrl : String
endpointUrl =
    "https://api.rollbar.com/api/1/item/"


send : Token -> Scope -> Environment -> Level -> String -> Dict String Value -> Task Http.Error Uuid
send token scope environment level message metadata =
    Time.now
        |> Task.andThen (sendWithTime token scope environment level message metadata)


sendWithTime : Token -> Scope -> Environment -> Level -> String -> Dict String Value -> Time -> Task Http.Error Uuid
sendWithTime token scope environment level message metadata time =
    let
        uuid =
            uuidFromTime time
    in
    { method = "POST"
    , headers = [ tokenHeader token ]
    , url = endpointUrl
    , body = toJsonBody token environment level message uuid metadata
    , expect = Http.expectStringResponse (\_ -> Ok ()) -- TODO
    , timeout = Nothing
    , withCredentials = False
    }
        |> Http.request
        -- TODO retry if rate limited
        |> Http.toTask
        |> Task.map (\() -> uuid)


uuidFromTime : Time -> Uuid
uuidFromTime time =
    time
        |> floor
        |> Random.initialSeed
        |> Random.step uuidGenerator
        |> Tuple.first


toJsonBody : Token -> Environment -> Level -> String -> Uuid -> Dict String Value -> Http.Body
toJsonBody (Token token) (Environment environment) level message uuid metadata =
    -- See https://rollbar.com/docs/api/items_post/ for schema
    [ ( "access_token", Json.Encode.string token )
    , ( "data"
      , Json.Encode.object
            [ ( "environment", Json.Encode.string environment )
            , ( "uuid", Uuid.encode uuid )
            , ( "notifier"
              , Json.Encode.object
                    [ ( "name", Json.Encode.string "elm-rollbar" )
                    , ( "version", Json.Encode.string version )
                    ]
              )
            , ( "level", Json.Encode.string (levelToString level) )
            , ( "endpoint", Json.Encode.string endpointUrl )
            , ( "platform", Json.Encode.string "browser" )
            , ( "language", Json.Encode.string "Elm" )
            , ( "body"
              , Json.Encode.object
                    [ ( "message"
                      , Json.Encode.object
                            (( "body", Json.Encode.string message ) :: Dict.toList metadata)
                      )
                    ]
              )
            ]
      )
    ]
        |> Json.Encode.object
        |> Http.jsonBody


tokenHeader : Token -> Http.Header
tokenHeader (Token token) =
    Http.header "X-Rollbar-Access-Token" token


{-| Return a Rollbar object scoped to a given filename and configured
to take ExtraInfo for both items above and below the error level.

    rollbar = Rollbar.scoped "Page/Home.elm"

    rollbar.debug "Hitting the hats API." Dict.empty

    [ ( "Payload", toString payload ) ]
        |> Dict.fromList
        |> rollbar.error "Unexpected payload from the hats API."

-}
scoped : Token -> Environment -> String -> Rollbar
scoped token environment scopeStr =
    let
        scope =
            Scope scopeStr
    in
    { critical = send token scope environment Critical
    , error = send token scope environment Error
    , warning = send token scope environment Warning
    , info = send token scope environment Info
    , debug = send token scope environment Debug
    }
