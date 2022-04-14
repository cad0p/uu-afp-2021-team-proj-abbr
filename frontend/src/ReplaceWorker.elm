port module ReplaceWorker exposing (..)

import Platform
import Shared exposing (FilePath, ShortHandrRequest(..), format)



-- MODEL


type alias Flags =
    { kbPath : FilePath }


type alias Model =
    { data : String, kbPath : FilePath }



-- INIT


init : Flags -> ( Model, Cmd Msg )
init { kbPath } =
    ( { data = "", kbPath = kbPath }, Cmd.none )



-- PROGRAM


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init -- receives flags and sets up the program
        , update = update -- handling model update
        , subscriptions = subscriptions -- handling subscriptions
        }



-- PORTS: communication to external services.
-- TODO:
--   [ ] Introduce a type-based message protocol instead of many parts
--   [ ] Get/send something fancier (like JSON) instead of Strings in the ports


{-| Default extension output port.

    - Worker - [String] -> Extension

-}
port toExtensionInfo : String -> Cmd msg



-- {-| Success extension output port.
--     - Worker - [String] -> Extension
-- -}
-- port toExtensionSuccess : String -> Cmd msg


{-| Error extension output port.

    - Worker - [String] -> Extension

-}
port toExtensionError : String -> Cmd msg


{-| Default extension input port.

    - Extension - [String] -> Worker

-}
port fromExtension : (String -> msg) -> Sub msg


{-| Extension Replace command requiest.

    - Extension - [FilePath] -> Worker

-}
port fromExtensionReplace : (FilePath -> msg) -> Sub msg


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



-- UPDATE


type Msg
    = ExtDataInput String
    | ExtRequestInput String
    | ShOkResult String
    | ShErrorResult String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ExtDataInput data ->
            ( { model | data = data }, toExtensionInfo "Pong!" )

        -- Extension request to replace content.
        ExtRequestInput file ->
            ( model, toShortHndr <| format (Replace { input = file, kbPath = model.kbPath, inplace = True }) )

        ShOkResult output ->
            ( model, toExtensionInfo output )

        ShErrorResult output ->
            ( model, toExtensionError output )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        -- TODO: add more type safety
        [ fromExtension ExtDataInput -- ^ Receive an info request from extension.
        , fromExtensionReplace ExtRequestInput -- ^ Receive an expand request from extension.
        , fromShortHndrSuccess ShOkResult -- ^ Receive an OK result from shorthndr.
        , fromShortHndrError ShErrorResult -- ^ Receive an Error result from shorthndr.
        ]
