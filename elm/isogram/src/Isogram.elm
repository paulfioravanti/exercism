module Isogram exposing (isIsogram)

import Set


isIsogram : String -> Bool
isIsogram sentence =
    let
        letters =
            sentence
                |> String.filter Char.isAlpha
                |> String.toLower

        uniqueLetters =
            letters
                |> String.toList
                |> Set.fromList
    in
    String.length letters == Set.size uniqueLetters
