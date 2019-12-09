module WordCount exposing (wordCount)

import Dict exposing (Dict)


wordCount : String -> Dict String Int
wordCount sentence =
    sentence
        |> convertToWords
        |> List.foldl incrementTallyForWord Dict.empty



-- PRIVATE


convertToWords : String -> List String
convertToWords sentence =
    let
        alphaNumOrSpaceOnly : Char -> Bool
        alphaNumOrSpaceOnly char =
            Char.isAlphaNum char || char == ' '
    in
    sentence
        |> String.toLower
        |> String.filter alphaNumOrSpaceOnly
        |> String.words


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
