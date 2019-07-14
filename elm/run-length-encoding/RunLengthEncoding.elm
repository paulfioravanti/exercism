module RunLengthEncoding exposing (decode, encode)

import Parser exposing ((|=), Parser, Step)
import Regex exposing (Regex)


type alias Encoding =
    ( Int, String )


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
