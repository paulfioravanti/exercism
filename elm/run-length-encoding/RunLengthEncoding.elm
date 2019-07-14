module RunLengthEncoding exposing (decode, encode)

import Regex exposing (Regex)


encode : String -> String
encode string =
    let
        consecutiveDataElements : Regex
        consecutiveDataElements =
            "([A-Za-z\\s])\\1+"
                |> Regex.fromString
                |> Maybe.withDefault Regex.never
    in
    Regex.replace consecutiveDataElements compress string


decode : String -> String
decode string =
    let
        runTimeEncoding : Regex
        runTimeEncoding =
            "\\d+\\D"
                |> Regex.fromString
                |> Maybe.withDefault Regex.never
    in
    Regex.replace runTimeEncoding reconstruct string



-- PRIVATE


compress : Regex.Match -> String
compress { match } =
    let
        character =
            match
                |> String.left 1

        count =
            match
                |> String.length
                |> String.fromInt
    in
    count ++ character


reconstruct : Regex.Match -> String
reconstruct { match } =
    let
        character =
            match
                |> String.right 1

        count =
            match
                |> String.slice 0 -1
                |> String.toInt
                |> Maybe.withDefault 0
    in
    String.repeat count character
