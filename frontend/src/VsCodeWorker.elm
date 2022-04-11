port module VsCodeWorker exposing (..)

import Html.Events exposing (..)
import Platform



-- PORTS


{-| Default extension output port.

    - Worker - [String] -> Extension

-}
port toExtension : String -> Cmd msg


{-| Default extension input port.

    - Extension - [String] -> Worker

-}
port fromExtension : (String -> msg) -> Sub msg


{-| Extension Extend command requiest.

    - Extension - [String] -> Worker

-}
port fromExtensionExtend : (String -> msg) -> Sub msg


{-| Default ShortHandr CLI trigger port.

    - Worker - [String] -> CLI

-}
port toShortHndr : String -> Cmd msg


{-| Default ShortHandr CLI result port.

    - Worker - [String] -> CLI

-}
port fromShortHndr : (String -> msg) -> Sub msg



-- MODEL


type alias Model =
    { lookup : String }



-- INIT


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { lookup = "" }, Cmd.none )



-- UPDATE


type alias Flags =
    {}


type Msg
    = ExtDataInput String
    | ExtReplaceInput String
    | ExtExtendInput String
    | SHResult String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ExtDataInput input ->
            ( { model | lookup = input }, toExtension "Pong!" )

        ExtReplaceInput input ->
            ( model, toShortHndr "--help" )

        ExtExtendInput input ->
            ( model, toShortHndr <| "extend --help --input=" ++ input )

        SHResult output ->
            ( model, toExtension output )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ fromExtension ExtDataInput
        , fromExtensionExtend ExtExtendInput
        , fromShortHndr SHResult
        ]



-- PROGRAM


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init -- receives flags and sets up the program
        , update = update -- handling model update
        , subscriptions = subscriptions -- handling subscriptions
        }
