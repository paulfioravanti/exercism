module Pangram exposing (isPangram)

import Set


isPangram : String -> Bool
isPangram sentence =
    let
        numberOfLettersInAlphabet =
            26
    in
    sentence
        |> String.toLower
        |> String.filter Char.isAlpha
        |> String.toList
        |> Set.fromList
        |> Set.size
        |> (==) numberOfLettersInAlphabet
