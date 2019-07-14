module RunLengthEncoding exposing (decode, encode)

import Parser exposing ((|=), Parser, Step)


type alias Encoding =
    { count : Int
    , character : String
    }


encode : String -> String
encode string =
    let
        compress : Encoding -> String
        compress { count, character } =
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
        reconstruct { count, character } =
            String.repeat count character
    in
    string
        |> Parser.run encodingsParser
        |> Result.withDefault []
        |> List.map reconstruct
        |> String.concat



-- PRIVATE


tallyEncoding : String -> List Encoding -> List Encoding
tallyEncoding character acc =
    case acc of
        [] ->
            [ { count = 1, character = character } ]

        head :: tail ->
            if character == head.character then
                { head | count = head.count + 1 } :: tail

            else
                { count = 1, character = character } :: head :: tail


encodingsParser : Parser (List Encoding)
encodingsParser =
    let
        encodingsIterationParser :
            List Encoding
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
    Parser.succeed Encoding
        |= countParser
        |= alphaOrSpaceParser
