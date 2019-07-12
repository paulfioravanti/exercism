module Pangram exposing (isPangram)

import Regex
import Set


isPangram : String -> Bool
isPangram sentence =
    let
        numberOfLettersInAlphabet =
            26

        nonAsciiLetters =
            "[^a-z]"
                |> Regex.fromString
                |> Maybe.withDefault Regex.never
    in
    sentence
        |> String.toLower
        |> Regex.replace nonAsciiLetters (always "")
        |> String.toList
        |> Set.fromList
        |> Set.size
        |> (==) numberOfLettersInAlphabet
