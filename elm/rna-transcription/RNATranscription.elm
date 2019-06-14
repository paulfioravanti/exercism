module RNATranscription exposing (toRNA)

import Dict exposing (Dict)


toRNA : String -> Result String String
toRNA dna =
    dna
        |> String.split ""
        |> List.foldl rnaTranscription ""
        |> Ok



-- PRIVATE


rnaTranscription : String -> String -> String
rnaTranscription dna acc =
    let
        transcription =
            rnaTranscriptions
                |> Dict.get dna
    in
    case transcription of
        Just rna ->
            acc ++ rna

        Nothing ->
            ""


rnaTranscriptions : Dict String String
rnaTranscriptions =
    Dict.fromList
        [ ( "A", "U" )
        , ( "C", "G" )
        , ( "G", "C" )
        , ( "T", "A" )
        ]
