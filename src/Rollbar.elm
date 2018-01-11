module Rollbar exposing (Environment, Level(..), Rollbar, Scope, Token, environment, scope, scoped, send, token)

import Http
import Json.Encode exposing (Value)
import Task exposing (Task)


version : String
version =
    "1.0.0"


type alias Rollbar =
    { critical : String -> List ( String, Value ) -> Task Http.Error ()
    , error : String -> List ( String, Value ) -> Task Http.Error ()
    , warning : String -> List ( String, Value ) -> Task Http.Error ()
    , info : String -> List ( String, Value ) -> Task Http.Error ()
    , debug : String -> List ( String, Value ) -> Task Http.Error ()
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


send : Token -> Scope -> Environment -> Level -> String -> List ( String, Value ) -> Task Http.Error ()
send token scope environment level message metadata =
    { method = "POST"
    , headers = [ tokenHeader token ]
    , url = endpointUrl
    , body = toJsonBody token environment level message metadata
    , expect = Http.expectStringResponse (\_ -> Ok ()) -- TODO
    , timeout = Nothing
    , withCredentials = False
    }
        |> Http.request
        -- TODO retry if rate limited
        |> Http.toTask


toJsonBody : Token -> Environment -> Level -> String -> List ( String, Value ) -> Http.Body
toJsonBody (Token token) (Environment environment) level message metadata =
    -- See https://rollbar.com/docs/api/items_post/ for schema
    [ ( "access_token", Json.Encode.string token )
    , ( "data"
      , Json.Encode.object
            [ ( "environment", Json.Encode.string environment )
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
                            (( "body", Json.Encode.string message ) :: metadata)
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

    rollbar.debug "Hitting the hats API." []

    rollbar.error "Unexpected payload from the hats API." [ ("Payload", toString payload) ]

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
