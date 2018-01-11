module Rollbar exposing (Level(..), Rollbar, scoped, send)

import Http
import Json.Encode exposing (Value)
import Task exposing (Task)


type alias Rollbar =
    { critical : String -> Value -> Task Http.Error ()
    , error : String -> Value -> Task Http.Error ()
    , warning : String -> Value -> Task Http.Error ()
    , info : String -> Value -> Task Http.Error ()
    , debug : String -> Value -> Task Http.Error ()
    }


type Level
    = Critical
    | Error
    | Warning
    | Info
    | Debug


type Token
    = Token String


type Scope
    = Scope String


toStringReportType : Level -> String
toStringReportType report =
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


send : Token -> Scope -> Level -> String -> Value -> Task Http.Error ()
send token scope level message payload =
    { method = "POST"
    , headers = [ tokenHeader token ]
    , url = "https://api.rollbar.com/api/1/item"
    , body = Http.jsonBody payload
    , expect = Http.expectStringResponse (\_ -> Ok ()) -- TODO
    , timeout = Nothing
    , withCredentials = False
    }
        |> Http.request
        -- TODO retry if rate limited
        |> Http.toTask


tokenHeader : Token -> Http.Header
tokenHeader (Token token) =
    Http.header "X-Rollbar-Access-Token" token


{-| Return a Rollbar object scoped to a given filename and configured
to take ExtraInfo for both items above and below the error level.

    rollbar = Rollbar.scoped "Page/Home.elm"

    rollbar.debug "Hitting the hats API." []

    rollbar.error "Unexpected payload from the hats API." [ ("Payload", toString payload) ]

-}
scoped : Token -> String -> Rollbar
scoped token scopeStr =
    let
        scope =
            Scope scopeStr
    in
    { critical = send token scope Critical
    , error = send token scope Error
    , warning = send token scope Warning
    , info = send token scope Info
    , debug = send token scope Debug
    }
