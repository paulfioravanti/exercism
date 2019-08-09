module Acronym exposing (abbreviate)

import Regex


abbreviate : String -> String
abbreviate phrase =
    let
        characterAfterAcronymTarget =
            {- negative lookahead - after asserting that what follows the
               current position is not a border character then a letter
               (ie an acronym target), match any single character
            -}
            "(?!\\b\\w)."
                |> Regex.fromString
                |> Maybe.withDefault Regex.never
    in
    phrase
        |> Regex.replace characterAfterAcronymTarget (always "")
        |> String.toUpper
