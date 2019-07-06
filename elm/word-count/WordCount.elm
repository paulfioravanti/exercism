module WordCount exposing (wordCount)

import Dict exposing (Dict)
import Regex


wordCount : String -> Dict String Int
wordCount sentence =
    sentence
        |> convertToWords
        |> List.foldl incrementTallyForWord Dict.empty



-- PRIVATE


convertToWords : String -> List String
convertToWords sentence =
    let
        wordRegex =
            "[^\\w-]"
                |> Regex.fromString
                |> Maybe.withDefault Regex.never
    in
    sentence
        |> String.toLower
        |> Regex.split wordRegex
        |> List.filter (not << String.isEmpty)


incrementTallyForWord : String -> Dict String Int -> Dict String Int
incrementTallyForWord word acc =
    let
        incrementTally : Maybe Int -> Maybe Int
        incrementTally tally =
            case tally of
                Just count ->
                    Just (count + 1)

                Nothing ->
                    Just 1
    in
    Dict.update word incrementTally acc
