module Acronym exposing (abbreviate)

import Regex


abbreviate : String -> String
abbreviate phrase =
    let
        characterAfterAcronymTarget =
            "(?!\\b\\w)."
                |> Regex.fromString
                |> Maybe.withDefault Regex.never
    in
    phrase
        |> Regex.replace characterAfterAcronymTarget (always "")
        |> String.toUpper
