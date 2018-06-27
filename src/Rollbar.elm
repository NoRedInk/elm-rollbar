module Rollbar exposing (Environment, Level(..), Rollbar, Scope, Token, environment, scope, scoped, send, token)

{-| Send reports to Rollbar.


## Types

@docs Rollbar, Level, Token, token, Environment, environment, Scope, scope


## Types

@docs scoped, send

-}

import Bitwise
import Dict exposing (Dict)
import FNV
import Http
import Json.Encode as Encode exposing (Value)
import Process
import Random.Pcg as Random
import Rollbar.Internal
import Task exposing (Task)
import Time exposing (Time)
import Uuid exposing (Uuid, uuidGenerator)


{-| Functions preapplied with access tokens, scopes, and environments,
separated by [`Level`](#Level).

Create one using [`scoped`](#scoped).

-}
type alias Rollbar =
    { critical : String -> Dict String Value -> Task Http.Error Uuid
    , error : String -> Dict String Value -> Task Http.Error Uuid
    , warning : String -> Dict String Value -> Task Http.Error Uuid
    , info : String -> Dict String Value -> Task Http.Error Uuid
    , debug : String -> Dict String Value -> Task Http.Error Uuid
    }


{-| Severity levels.
-}
type Level
    = Critical
    | Error
    | Warning
    | Info
    | Debug


{-| A Rollbar API access token.

Create one using [`token`](#token).

    Rollbar.token "12c99de67a444c229fca100e0967486f"

-}
type Token
    = Token String


{-| A scope, for example ``"login"`.

Create one using [`scope`](#scope).

    Rollbar.scope "login"

-}
type Scope
    = Scope String


{-| Create a [`Scope`](#Scope).

    Rollbar.scope "login"

-}
scope : String -> Scope
scope =
    Scope


{-| For example, "production", "development", or "staging".

Create one using [`environment`](#environment).

    Rollbar.environment "production"

-}
type Environment
    = Environment String


{-| Create a [`Token`](#token)

    Rollbar.token "12c99de67a444c229fca100e0967486f"

-}
token : String -> Token
token =
    Token


{-| Create an [`Environment`](#Environment)

    Rollbar.environment "production"

-}
environment : String -> Environment
environment =
    Environment


{-| Send a message to Rollbar. [`scoped`](#scoped)
provides a nice wrapper around this.

Arguments:

  - `Token` - The [Rollbar API token](https://rollbar.com/docs/api/#authentication) required to authenticate the request.
  - `Scope` - Scoping messages essentially namespaces them. For example, this might be the name of the page the user was on when the message was sent.
  - `Environment` - e.g. `"production"`, `"development"`, `"staging"`, etc.
  - `Int` - maximum retry attempts - if the response is that the message was rate limited, try resending again (once per second) up to this many times. (0 means "do not retry.")
  - `Level` - severity, e.g. `Error`, `Warning`, `Debug`
  - `String` - message, e.g. "Auth server was down when user tried to sign in."
  - `Dict String Value` - arbitrary metadata, e.g. `{"username": "rtfeldman"}`

If the message was successfully sent to Rollbar, the [`Task`](http://package.elm-lang.org/packages/elm-lang/core/latest/Task#Task)
succeeds with the [`Uuid`](http://package.elm-lang.org/packages/danyx23/elm-uuid/latest/Uuid#Uuid)
it generated and sent to Rollbar to identify the message. Otherwise it fails
with the [`Http.Error`](http://package.elm-lang.org/packages/elm-lang/http/latest/Http#Error)
responsible.

-}
send : Token -> Scope -> Environment -> Int -> Level -> String -> Dict String Value -> Task Http.Error Uuid
send token scope environment maxRetryAttempts level message metadata =
    Time.now
        |> Task.andThen (sendWithTime token scope environment maxRetryAttempts level message metadata)



-- INTERNAL --


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


sendWithTime : Token -> Scope -> Environment -> Int -> Level -> String -> Dict String Value -> Time -> Task Http.Error Uuid
sendWithTime token scope environment maxRetryAttempts level message metadata time =
    let
        uuid : Uuid
        uuid =
            uuidFrom token scope environment level message metadata time

        body : Http.Body
        body =
            toJsonBody token environment level message uuid metadata
    in
    { method = "POST"
    , headers = [ tokenHeader token ]
    , url = endpointUrl
    , body = body
    , expect = Http.expectStringResponse (\_ -> Ok ()) -- TODO
    , timeout = Nothing
    , withCredentials = False
    }
        |> Http.request
        |> Http.toTask
        |> Task.map (\() -> uuid)
        |> withRetry maxRetryAttempts


withRetry : Int -> Task Http.Error a -> Task Http.Error a
withRetry maxRetryAttempts task =
    let
        retry : Http.Error -> Task Http.Error a
        retry httpError =
            if maxRetryAttempts > 0 then
                case httpError of
                    Http.BadStatus { status } ->
                        if status.code == 429 then
                            -- Wait a bit between retries.
                            Process.sleep retries.msDelayBetweenRetries
                                |> Task.andThen (\() -> withRetry (maxRetryAttempts - 1) task)
                        else
                            Task.fail httpError

                    _ ->
                        Task.fail httpError
            else
                Task.fail httpError
    in
    Task.onError retry task


{-| Using the current system time as a random number seed generator, generate a
UUID.

We could theoretically generate the same UUID twice if we tried to send
two messages in extremely rapid succession. To guard against this, we
incorporate the contents of the message in the random number seed so that the
only way we could expect the same UUID is if we were sending a duplicate
message.

-}
uuidFrom : Token -> Scope -> Environment -> Level -> String -> Dict String Value -> Time -> Uuid
uuidFrom (Token token) (Scope scope) (Environment environment) level message metadata time =
    let
        hash : Int
        hash =
            [ Encode.string (levelToString level)
            , Encode.string message
            , Encode.string token
            , Encode.string scope
            , Encode.string environment
            , Dict.toList metadata
                |> List.map (\( key, value ) -> Encode.list [ Encode.string key, value ])
                |> Encode.list
            ]
                |> Encode.list
                |> Encode.encode 0
                |> FNV.hashString

        combinedSeed =
            Bitwise.xor (floor time) hash
    in
    Random.initialSeed combinedSeed
        |> Random.step uuidGenerator
        |> Tuple.first


toJsonBody : Token -> Environment -> Level -> String -> Uuid -> Dict String Value -> Http.Body
toJsonBody (Token token) (Environment environment) level message uuid metadata =
    -- See https://rollbar.com/docs/api/items_post/ for schema
    [ ( "access_token", Encode.string token )
    , ( "data"
      , Encode.object
            [ ( "environment", Encode.string environment )
            , ( "uuid", Uuid.encode uuid )
            , ( "notifier"
              , Encode.object
                    [ ( "name", Encode.string "elm-rollbar" )
                    , ( "version", Encode.string Rollbar.Internal.version )
                    ]
              )
            , ( "level", Encode.string (levelToString level) )
            , ( "endpoint", Encode.string endpointUrl )
            , ( "platform", Encode.string "browser" )
            , ( "language", Encode.string "Elm" )
            , ( "body"
              , Encode.object
                    [ ( "message"
                      , Encode.object
                            (( "body", Encode.string message ) :: Dict.toList metadata)
                      )
                    ]
              )
            ]
      )
    ]
        |> Encode.object
        |> Http.jsonBody


tokenHeader : Token -> Http.Header
tokenHeader (Token token) =
    Http.header "X-Rollbar-Access-Token" token


{-| Return a [`Rollbar`](#Rollbar) record configured with the given
[`Environment`](#Environment) and [`Scope`](#Scope) string.

If the HTTP request to Rollbar fails because of an exceeded rate limit (status
code 429), this will retry the HTTP request once per second, up to 60 times.

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
    { critical = send token scope environment retries.defaultMaxAttempts Critical
    , error = send token scope environment retries.defaultMaxAttempts Error
    , warning = send token scope environment retries.defaultMaxAttempts Warning
    , info = send token scope environment retries.defaultMaxAttempts Info
    , debug = send token scope environment retries.defaultMaxAttempts Debug
    }


{-| According to <https://rollbar.com/docs/rate-limits/>
the default rate limit for all access tokens is 5,000 calls per minute.
This window resets every minute, so retry after waiting 1 sec, and default to
retrying up to 60 times.
-}
retries : { defaultMaxAttempts : Int, msDelayBetweenRetries : Time }
retries =
    { defaultMaxAttempts = 60
    , msDelayBetweenRetries = 1000
    }


endpointUrl : String
endpointUrl =
    "https://api.rollbar.com/api/1/item/"
