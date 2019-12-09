module Etl exposing (transform)

import Dict exposing (Dict)


transform : Dict Int (List String) -> Dict String Int
transform input =
    input
        |> Dict.foldl transformWords Dict.empty



-- PRIVATE


transformWords : Int -> List String -> Dict String Int -> Dict String Int
transformWords score words scores =
    words
        |> List.foldl (scoreByWord score) scores


scoreByWord : Int -> String -> Dict String Int -> Dict String Int
scoreByWord score word scores =
    scores
        |> Dict.insert (String.toLower word) score
