module Say exposing (SayError(..), say)

import Dict exposing (Dict)


type SayError
    = Negative
    | TooLarge


say : Int -> Result SayError String
say number =
    if isTooSmall number then
        Err Negative

    else if isTooLarge number then
        Err TooLarge

    else if isZero number then
        Ok "zero"

    else if isUpToTwenty number then
        numberWords
            |> Dict.get number
            |> Result.fromMaybe Negative

    else if isUpToNinetyNine number then
        Ok (hypenatedWord number)

    else
        Ok (fullWord number)



-- PRIVATE


hypenatedWord : Int -> String
hypenatedWord number =
    let
        onesValue =
            case digits [] number of
                _ :: ones :: [] ->
                    ones

                _ ->
                    0

        tensWord =
            numberWords
                |> Dict.get (number - onesValue)
                |> Maybe.withDefault ""

        onesWord =
            numberWords
                |> Dict.get onesValue
                |> Maybe.withDefault ""
    in
    tensWord ++ "-" ++ onesWord


fullWord : Int -> String
fullWord number =
    if isZero number then
        ""

    else if isUpToTwenty number then
        numberWords
            |> Dict.get number
            |> Maybe.withDefault ""

    else if isUpToNinetyNine number then
        hypenatedWord number

    else if isTenThousandUpToOneMillion number then
        splitListByScale 3 number

    else if isTenMillionUpToOneBillion number then
        splitListByScale 6 number

    else if isTenBillionUpToOneTrillion number then
        splitListByScale 9 number

    else
        constructFullWord number


constructFullWord : Int -> String
constructFullWord number =
    let
        ( headDigit, tailDigits ) =
            case digits [] number of
                head :: tail ->
                    ( head, tail )

                _ ->
                    ( 0, [] )

        headWord =
            numberWords
                |> Dict.get headDigit
                |> Maybe.withDefault ""

        scale =
            scales
                |> Dict.get (List.length tailDigits)
                |> Maybe.withDefault ""

        tailWords =
            tailDigits
                |> undigits 0
                |> fullWord
                |> formatTailWord tailDigits
    in
    headWord ++ " " ++ scale ++ tailWords


formatTailWord : List Int -> String -> String
formatTailWord tail word =
    let
        numRemainingNonZeroDigits =
            tail
                |> dropWhile (\int -> int == 0)
                |> List.length
    in
    case numRemainingNonZeroDigits of
        0 ->
            ""

        1 ->
            " and " ++ word

        2 ->
            " and " ++ word

        _ ->
            " " ++ word


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

        scaleWord =
            scales
                |> Dict.get scale
                |> Maybe.withDefault ""
    in
    head ++ " " ++ scaleWord ++ " " ++ tail


digits : List Int -> Int -> List Int
digits acc int =
    let
        base =
            10
    in
    if abs int < base then
        int :: acc

    else
        let
            {- Integer division won't work because of this weirdness:
               > 987654321123 // 10
               -18815696
            -}
            flooredInt =
                floor (toFloat int / base)
        in
        digits (remainderBy base int :: acc) flooredInt


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


dropWhile : (a -> Bool) -> List a -> List a
dropWhile predicate list =
    case list of
        [] ->
            []

        head :: tail ->
            if predicate head then
                dropWhile predicate tail

            else
                list


isTooSmall : Int -> Bool
isTooSmall number =
    number < 0


isTooLarge : Int -> Bool
isTooLarge number =
    number > 999999999999


isZero : Int -> Bool
isZero number =
    number == 0


isUpToTwenty : Int -> Bool
isUpToTwenty number =
    number < 21


isUpToNinetyNine : Int -> Bool
isUpToNinetyNine number =
    number < 100


isTenThousandUpToOneMillion : Int -> Bool
isTenThousandUpToOneMillion number =
    number > 9999 && number < 1000000


isTenMillionUpToOneBillion : Int -> Bool
isTenMillionUpToOneBillion number =
    number > 9999999 && number < 1000000000


isTenBillionUpToOneTrillion : Int -> Bool
isTenBillionUpToOneTrillion number =
    number > 9999999999 && number < 1000000000000


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
