port module VsCodeWorker exposing (..)

import Html.Events exposing (..)
import Platform



-- PORTS


port toExtension : String -> Cmd msg


port fromExtension : (String -> msg) -> Sub msg



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
    = SendData String
    | Received String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendData output ->
            ( model, toExtension output )

        Received input ->
            ( { model | lookup = input }, toExtension "back!" )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    fromExtension Received



-- PROGRAM


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init -- receives flags and sets up the program
        , update = update -- handling model update
        , subscriptions = subscriptions -- handling subscriptions
        }
