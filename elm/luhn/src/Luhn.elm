module Luhn exposing (valid)


valid : String -> Bool
valid input =
    let
        number =
            input
                |> String.replace " " ""
    in
    isValidFormat number && isValidValue number



-- PRIVATE


isValidFormat : String -> Bool
isValidFormat number =
    String.length number > 1 && String.all Char.isDigit number


isValidValue : String -> Bool
isValidValue number =
    let
        isEvenlyDivisibleBy10 : Int -> Bool
        isEvenlyDivisibleBy10 num =
            remainderBy 10 num == 0
    in
    number
        |> toReversedNumbers
        |> chunkEvery 2
        |> List.foldl addLuhnValue 0
        |> isEvenlyDivisibleBy10


toReversedNumbers : String -> List Int
toReversedNumbers number =
    number
        |> String.reverse
        |> String.split ""
        |> List.filterMap String.toInt


chunkEvery : Int -> List Int -> List (List Int)
chunkEvery size list =
    if size >= List.length list then
        [ list ]

    else
        let
            chunk =
                List.take size list

            tail =
                List.drop size list
        in
        chunk :: chunkEvery size tail


addLuhnValue : List Int -> Int -> Int
addLuhnValue chunk acc =
    case chunk of
        [ leftValue ] ->
            acc + leftValue

        [ leftValue, rightValue ] ->
            let
                double : Int -> Int
                double number =
                    number * 2

                subtract9IfGreaterThan9 : Int -> Int
                subtract9IfGreaterThan9 number =
                    if number > 9 then
                        number - 9

                    else
                        number

                rightLuhnValue =
                    rightValue
                        |> double
                        |> subtract9IfGreaterThan9
            in
            acc + leftValue + rightLuhnValue

        _ ->
            acc
