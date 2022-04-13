module Shared exposing (..)

-- ALIASES


type alias FilePath =
    String



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

        Replace { input, kbPath, inplace } ->
            "replace --input=\""
                ++ input
                ++ "\" -k=\""
                ++ kbPath
                ++ "\" "
                ++ (if inplace then
                        "--inplace"

                    else
                        ""
                   )

        Help _ ->
            "--help"
