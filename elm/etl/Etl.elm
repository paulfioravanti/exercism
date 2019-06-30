module Etl exposing (transform)

import Dict exposing (Dict)


transform : Dict Int (List String) -> Dict String Int
transform input =
    input
        |> Dict.toList
        |> List.foldl transformWords []
        |> Dict.fromList



-- PRIVATE


transformWords :
    ( Int, List String )
    -> List ( String, Int )
    -> List ( String, Int )
transformWords ( score, words ) scores =
    words
        |> List.foldl (scoreByWord score) scores


scoreByWord : Int -> String -> List ( String, Int ) -> List ( String, Int )
scoreByWord score word scores =
    ( String.toLower word, score ) :: scores
