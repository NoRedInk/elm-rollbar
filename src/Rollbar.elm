effect module Rollbar
    where { command = RollbarCmd }
    exposing
        ( Rollbar
        , scopedRollbar
        , crash
        )

import Json.Encode as Json
import Native.Rollbar
import Task exposing (Task)


type ReportType
    = Critical
    | Error
    | Warning
    | Info
    | Debug


toStringReportType : ReportType -> String
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


-- I believe this is new best practice
-- if you have to wrap a JS value in a type
-- then use Json.Value to signify this
-- prevents it breaking on various built in elm things


type RollbarObject
    = RollbarObject Json.Value


type alias Rollbar msg =
    { critical : String -> Cmd msg
    , error : String -> Cmd msg
    , warning : String -> Cmd msg
    , info : String -> Cmd msg
    , debug : String -> Cmd msg
    }


{-|
report to github as critical, then call debug.crash
-}
crash : Rollbar msg -> String -> a
crash rollbar =
    Native.Rollbar.crash (Debug.crash) (Native.Rollbar.scopedRollbar rollbar)


{-|
use a scoped rollbar to report a message at the given level
-}
report : RollbarObject -> ReportType -> String -> Task Never ()
report rollbar reportType message =
    let
        type' =
            toStringReportType reportType
                |> Json.string
    in
        Native.Rollbar.report rollbar type' message


{-|
return a rollbar object scoped with a given filename
-}
scopedRollbar : String -> Rollbar msg
scopedRollbar filename =
    let
        rollbar =
            Native.Rollbar.scopedRollbar filename

        reportEffects =
            send rollbar
    in
        { critical = reportEffects Critical
        , error = reportEffects Error
        , warning = reportEffects Warning
        , info = reportEffects Info
        , debug = reportEffects Debug
        }



-- Effects Manager


send : RollbarObject -> ReportType -> String -> Cmd msg
send rollbar reportType message =
    command (Send rollbar reportType message)


type RollbarCmd msg
    = Send RollbarObject ReportType String


cmdMap : (a -> b) -> RollbarCmd a -> RollbarCmd b
cmdMap _ (Send rollbar reportType message) =
    Send rollbar reportType message


type RollbarState
    = RollbarState


init : Task Never RollbarState
init =
    Task.succeed RollbarState


type alias RollbarMsg =
    Never


onEffects : Platform.Router msg RollbarMsg -> List (RollbarCmd msg) -> RollbarState -> Task Never RollbarState
onEffects router cmds state =
    cmds
        |> List.map onEffect
        |> Task.sequence
        |> Task.map (always RollbarState)


onEffect : RollbarCmd msg -> Task Never ()
onEffect cmd =
    case cmd of
        Send rollbar reportType message ->
            report rollbar reportType message


onSelfMsg : Platform.Router msg RollbarMsg -> RollbarMsg -> RollbarState -> Task Never RollbarState
onSelfMsg router selfMsg state =
    Task.succeed RollbarState
