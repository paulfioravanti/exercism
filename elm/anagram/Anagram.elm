module Anagram exposing (detect)


detect : String -> List String -> List String
detect word candidates =
    let
        baseWord =
            String.toLower word

        letters =
            toSortedLetters baseWord
    in
    candidates
        |> List.filter (isAnagram baseWord letters)



-- PRIVATE


toSortedLetters : String -> List String
toSortedLetters word =
    word
        |> String.split ""
        |> List.sort


isAnagram : String -> List String -> String -> Bool
isAnagram word letters candidate =
    let
        candidateWord =
            String.toLower candidate
    in
    not (isSameWord word candidateWord) && areSameLetters letters candidateWord


isSameWord : String -> String -> Bool
isSameWord word candidate =
    word == candidate


areSameLetters : List String -> String -> Bool
areSameLetters letters candidate =
    letters == toSortedLetters candidate
