module Isogram exposing (isIsogram)

import Set


isIsogram : String -> Bool
isIsogram sentence =
    let
        letters =
            sentence
                |> String.filter Char.isAlpha
                |> String.toLower
                |> String.toList

        uniqueLetters =
            Set.fromList letters
    in
    List.length letters == Set.size uniqueLetters
