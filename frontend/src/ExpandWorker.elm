port module ExpandWorker exposing (..)

import Platform



-- ALIASES


type alias FilePath =
    String



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



-- PORTS


{-| Default extension output port.

    - Worker - [String] -> Extension

-}
port toExtension : String -> Cmd msg


{-| Default extension input port.

    - Extension - [String] -> Worker

-}
port fromExtension : (String -> msg) -> Sub msg



-- {-| Default extension config port.
--     - Extension - [Config] -> Worker
--     -- TODO: support JSON input?
-- -}
-- port fromExtensionConfig : (String -> msg) -> Sub msg


{-| Extension Expand command requiest.

    - Extension - [String] -> Worker

-}
port fromExtensionExpand : (String -> msg) -> Sub msg


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



-- REQUESTS


{-| Supported ShortHandr request requests.
TODO: generate this automatically?
-}
type ShortHandrRequest
    = Expand { abbreviation : String, kbPath : FilePath }
    | Replace { input : FilePath, kbPath : FilePath, inplace : Bool }
    | Help { command : String }


{-| Format the command input according to procol
-}
format : ShortHandrRequest -> String
format p =
    case p of
        Expand { abbreviation, kbPath } ->
            "expand --abbreviation=\"" ++ abbreviation ++ "\" -k=\"" ++ kbPath ++ "\""

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
        ExtDataInput data ->
            ( { model | data = data }, toExtension "Pong!" )

        -- Extension request to replace content.
        ExtReplaceInput _ ->
            ( model, toShortHndr <| format (Replace { input = "", kbPath = "", inplace = True }) )

        -- Extension request to expand content.
        ExtExpandInput request ->
            ( model, toShortHndr <| format (Expand { abbreviation = request, kbPath = model.kbPath }) )

        ShOkResult output ->
            ( model, toExtension output )

        ShErrorResult output ->
            ( model, toExtension output )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        -- TODO: add more type safety
        [ fromExtension ExtDataInput -- ^ Receive an info request from extension.
        , fromExtensionExpand ExtExpandInput -- ^ Receive an expand request from extension.
        , fromShortHndrSuccess ShOkResult -- ^ Receive an OK result from shorthndr.
        , fromShortHndrError ShErrorResult -- ^ Receive an Error result from shorthndr.
        ]
