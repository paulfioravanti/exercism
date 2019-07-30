module Isogram exposing (isIsogram)

import Set


isIsogram : String -> Bool
isIsogram sentence =
    let
        letters =
            sentence
                |> String.toLower
                |> String.toList
                |> List.filter Char.isAlpha

        uniqueLetters =
            Set.fromList letters
    in
    List.length letters == Set.size uniqueLetters
