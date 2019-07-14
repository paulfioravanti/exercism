module RunLengthEncoding exposing (decode, encode)

import Regex


encode : String -> String
encode string =
    let
        consecutiveDataElements =
            "([A-Za-z\\s])\\1+"
                |> Regex.fromString
                |> Maybe.withDefault Regex.never
    in
    Regex.replace consecutiveDataElements compress string


decode : String -> String
decode string =
    let
        runTimeEncoding =
            "(\\d+)(\\D)"
                |> Regex.fromString
                |> Maybe.withDefault Regex.never
    in
    Regex.replace runTimeEncoding reconstruct string



-- PRIVATE


compress : Regex.Match -> String
compress match =
    let
        character =
            case match.submatches of
                letter :: _ ->
                    Maybe.withDefault "" letter

                _ ->
                    ""

        count =
            match.match
                |> String.length
                |> String.fromInt
    in
    count ++ character


reconstruct : Regex.Match -> String
reconstruct match =
    let
        ( count, character ) =
            case match.submatches of
                int :: char :: _ ->
                    let
                        number =
                            int
                                |> Maybe.withDefault ""
                                |> String.toInt
                                |> Maybe.withDefault 0

                        letter =
                            Maybe.withDefault "" char
                    in
                    ( number, letter )

                _ ->
                    ( 0, "" )
    in
    character
        |> String.repeat count
