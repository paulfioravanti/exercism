module Say exposing (SayError(..), say)

import Dict exposing (Dict)


type SayError
    = Negative
    | TooLarge


say : Int -> Result SayError String
say number =
    if number < 0 then
        Err Negative

    else if number > 999999999999 then
        Err TooLarge

    else if number == 0 then
        Ok "zero"

    else if number < 21 then
        numberWords
            |> Dict.get number
            |> Result.fromMaybe Negative

    else if number < 100 then
        Ok (hypenatedWord number)

    else
        Ok (fullWord number)



-- PRIVATE


hypenatedWord : Int -> String
hypenatedWord number =
    let
        ones =
            case digits [] number of
                _ :: ones_ :: [] ->
                    ones_

                _ ->
                    0

        tensWord =
            numberWords
                |> Dict.get (number - ones)
                |> Maybe.withDefault ""

        onesWord =
            numberWords
                |> Dict.get ones
                |> Maybe.withDefault ""
    in
    tensWord ++ "-" ++ onesWord


digits : List Int -> Int -> List Int
digits acc int =
    let
        base =
            10
    in
    if abs int < base then
        int :: acc

    else
        digits (remainderBy base int :: acc) (int // base)


undigits : Int -> List Int -> Int
undigits acc digitList =
    let
        base =
            10
    in
    case digitList of
        [] ->
            acc

        head :: tail ->
            undigits (acc * base + head) tail


fullWord : Int -> String
fullWord number =
    if number == 0 then
        ""

    else if number < 21 then
        numberWords
            |> Dict.get number
            |> Maybe.withDefault ""

    else if number < 100 then
        hypenatedWord number

    else if number > 999 && number < 1000000 then
        splitListByScale 3 number

    else
        let
            ( head, tail ) =
                case digits [] number of
                    head_ :: tail_ ->
                        ( head_, tail_ )

                    _ ->
                        ( 0, [] )

            headWord =
                numberWords
                    |> Dict.get head
                    |> Maybe.withDefault ""

            scale =
                scales
                    |> Dict.get (List.length tail)
                    |> Maybe.withDefault ""

            tailWords =
                tail
                    |> undigits 0
                    |> fullWord
                    |> formatTailWord
        in
        headWord ++ " " ++ scale ++ tailWords


formatTailWord : String -> String
formatTailWord word =
    case word of
        "" ->
            ""

        _ ->
            " and " ++ word


splitListByScale : Int -> Int -> String
splitListByScale scale number =
    let
        digitList =
            digits [] number

        scaleChunk =
            List.length digitList - scale

        head =
            digitList
                |> List.take scaleChunk
                |> undigits 0
                |> fullWord

        tail =
            digitList
                |> List.drop scaleChunk
                |> undigits 0
                |> fullWord

        scale_ =
            scales
                |> Dict.get scale
                |> Maybe.withDefault ""
    in
    head ++ " " ++ scale_ ++ " " ++ tail


scales : Dict Int String
scales =
    Dict.fromList
        [ ( 2, "hundred" )
        , ( 3, "thousand" )
        , ( 6, "million" )
        , ( 9, "billion" )
        ]


numberWords : Dict Int String
numberWords =
    Dict.fromList
        [ ( 1, "one" )
        , ( 2, "two" )
        , ( 3, "three" )
        , ( 4, "four" )
        , ( 5, "five" )
        , ( 6, "six" )
        , ( 7, "seven" )
        , ( 8, "eight" )
        , ( 9, "nine" )
        , ( 10, "ten" )
        , ( 11, "eleven" )
        , ( 12, "twelve" )
        , ( 13, "thirteen" )
        , ( 14, "fourteen" )
        , ( 15, "fifteen" )
        , ( 16, "sixteen" )
        , ( 17, "seventeen" )
        , ( 18, "eighteen" )
        , ( 19, "nineteen" )
        , ( 20, "twenty" )
        , ( 30, "thirty" )
        , ( 40, "forty" )
        , ( 50, "fifty" )
        , ( 60, "sixty" )
        , ( 70, "seventy" )
        , ( 80, "eighty" )
        , ( 90, "ninety" )
        ]
