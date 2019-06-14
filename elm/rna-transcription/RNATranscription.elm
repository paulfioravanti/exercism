module RNATranscription exposing (toRNA)

import Dict exposing (Dict)


toRNA : String -> Result Char String
toRNA dna =
    dna
        |> String.foldr rnaTranscription (Ok "")



-- PRIVATE


rnaTranscription : Char -> Result Char String -> Result Char String
rnaTranscription dna acc =
    let
        transcription =
            rnaTranscriptions
                |> Dict.get dna
    in
    case transcription of
        Just rna ->
            acc
                |> Result.map (String.cons rna)

        Nothing ->
            Err dna


rnaTranscriptions : Dict Char Char
rnaTranscriptions =
    Dict.fromList
        [ ( 'A', 'U' )
        , ( 'C', 'G' )
        , ( 'G', 'C' )
        , ( 'T', 'A' )
        ]
