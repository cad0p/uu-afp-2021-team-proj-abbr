port module VsCodeWorker exposing (..)

import Html exposing (a, input)
import Platform



-- MODEL


type alias Flags =
    {}


type alias Model =
    { input : String }



-- INIT


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { input = "" }, Cmd.none )



-- PROGRAM


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init -- receives flags and sets up the program
        , update = update -- handling model update
        , subscriptions = subscriptions -- handling subscriptions
        }



-- PORTS
-- TODO: figure out how to make it more type safe.


{-| Default extension output port.

    - Worker - [String] -> Extension

-}
port toExtension : String -> Cmd msg


{-| Default extension input port.

    - Extension - [String] -> Worker

-}
port fromExtension : (String -> msg) -> Sub msg


{-| Extension Expand command requiest.

    - Extension - [String] -> Worker

-}
port fromExtensionExpand : (String -> msg) -> Sub msg


{-| Default ShortHandr CLI trigger port.

    - Worker - [String] -> CLI

-}
port toShortHndr : String -> Cmd msg


{-| OK ShortHandr CLI result port.

    - Worker - [String] -> CLI

-}
port fromShortHndrSuccess : (String -> msg) -> Sub msg


{-| Error ShortHandr CLI result port.

    - Worker - [String] -> CLI

-}
port fromShortHndrError : (String -> msg) -> Sub msg



-- PROTOCOLS
-- TODO: idea for the future - make it automated?


type ShortHandrProtocol i
    = Expand i
    | Replace i
    | Help i


{-| Format the command input according to procol
-}
format : ShortHandrProtocol String -> String
format p =
    case p of
        Expand input ->
            "expand --abbreviation=\"" ++ input ++ "\""

        Replace _ ->
            "replace --help"

        Help _ ->
            "--help"



-- UPDATE


type Msg
    = ExtDataInput String
    | ExtReplaceInput String
    | ExtExpandInput String
    | ShOkResult String
    | ShErrorResult String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ExtDataInput input ->
            ( { model | input = input }, toExtension "Pong!" )

        ExtReplaceInput _ ->
            ( model, toShortHndr <| format (Replace "") )

        ExtExpandInput input ->
            ( model, toShortHndr <| format (Expand input) )

        ShOkResult output ->
            ( model, toExtension output )

        ShErrorResult output ->
            ( model, toExtension output )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ fromExtension ExtDataInput
        , fromExtensionExpand ExtExpandInput -- TODO: add more type safety
        , fromShortHndrSuccess ShOkResult -- TODO: add more type safety
        , fromShortHndrError ShErrorResult -- TODO: add more type safety
        ]
