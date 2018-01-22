module Rollbar exposing (Environment, Level(..), Rollbar, Scope, Token, environment, scope, scoped, send, token)

{-| Send reports to Rollbar.


## Types

@docs Rollbar, Level, Token, token, Environment, environment, Scope, scope


## Types

@docs scoped, send

-}

import Dict exposing (Dict)
import Http
import Json.Encode exposing (Value)
import Process
import Random.Pcg as Random
import Rollbar.Internal exposing (version)
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
        |> Task.andThen
            (\time ->
                sendWithUuid token
                    scope
                    environment
                    maxRetryAttempts
                    level
                    message
                    metadata
                    (uuidFromTime time)
            )



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


sendWithUuid : Token -> Scope -> Environment -> Int -> Level -> String -> Dict String Value -> Uuid -> Task Http.Error Uuid
sendWithUuid token scope environment maxRetryAttempts level message metadata uuid =
    let
        request : Http.Request ()
        request =
            Http.request
                { method = "POST"
                , headers = [ tokenHeader token ]
                , url = endpointUrl
                , body = toJsonBody token environment level message uuid metadata
                , expect = Http.expectStringResponse (\_ -> Ok ()) -- TODO
                , timeout = Nothing
                , withCredentials = False
                }

        retry : Http.Error -> Task Http.Error Uuid
        retry httpError =
            if maxRetryAttempts > 0 then
                case httpError of
                    Http.BadStatus { status } ->
                        if status.code == 429 then
                            -- Wait a bit between retries.
                            Process.sleep retries.msDelayBetweenRetries
                                |> Task.andThen
                                    (\() ->
                                        -- Retry using the same UUID as before.
                                        sendWithUuid token
                                            scope
                                            environment
                                            (maxRetryAttempts - 1)
                                            level
                                            message
                                            metadata
                                            uuid
                                    )
                        else
                            Task.fail httpError

                    _ ->
                        Task.fail httpError
            else
                Task.fail httpError
    in
    request
        |> Http.toTask
        |> Task.map (\() -> uuid)
        |> Task.onError retry


{-| Using the current system time as a random number seed generator, generate a
UUID.

TODO: We could theoretically generate the same UUID twice if we tried to send
two messages in extremely rapid succession. To guard against this, we could
incorporate other sources of entropy into the random number generation.

-}
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
