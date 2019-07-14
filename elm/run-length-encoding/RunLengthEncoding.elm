module RunLengthEncoding exposing (decode, encode)

import Parser exposing ((|=), Parser, Step)


type alias Encoding =
    ( Int, String )


encode : String -> String
encode string =
    let
        compress : Encoding -> String
        compress ( count, character ) =
            case count of
                1 ->
                    character

                _ ->
                    String.fromInt count ++ character
    in
    string
        |> String.split ""
        |> List.foldr tallyEncoding []
        |> List.map compress
        |> String.concat


decode : String -> String
decode string =
    let
        reconstruct : Encoding -> String
        reconstruct ( count, character ) =
            String.repeat count character
    in
    string
        |> Parser.run encodingsParser
        |> Result.withDefault []
        |> List.map reconstruct
        |> String.concat



-- PRIVATE


tallyEncoding : String -> List Encoding -> List Encoding
tallyEncoding char acc =
    case acc of
        [] ->
            [ ( 1, char ) ]

        ( count, character ) :: tail ->
            if character == char then
                ( count + 1, character ) :: tail

            else
                ( 1, char ) :: ( count, character ) :: tail


encodingsParser : Parser (List Encoding)
encodingsParser =
    let
        encodingsIterationParser :
            List ( Int, String )
            -> Parser (Step (List Encoding) (List Encoding))
        encodingsIterationParser encodings =
            Parser.oneOf
                [ Parser.succeed
                    (\encoding -> Parser.Loop (encoding :: encodings))
                    |= encodingParser
                , Parser.succeed ()
                    |> Parser.map (\_ -> Parser.Done (List.reverse encodings))
                ]
    in
    Parser.loop [] encodingsIterationParser


encodingParser : Parser Encoding
encodingParser =
    let
        countParser : Parser Int
        countParser =
            Parser.oneOf
                [ Parser.succeed identity
                    |= Parser.int
                , Parser.succeed 1
                ]

        alphaOrSpaceParser : Parser String
        alphaOrSpaceParser =
            let
                isAlphaOrSpace char =
                    Char.isAlpha char || char == ' '
            in
            Parser.chompIf isAlphaOrSpace
                |> Parser.getChompedString
    in
    Parser.succeed Tuple.pair
        |= countParser
        |= alphaOrSpaceParser
