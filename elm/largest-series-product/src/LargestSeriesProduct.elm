module LargestSeriesProduct exposing (largestProduct)


largestProduct : Int -> String -> Maybe Int
largestProduct length series =
    if isInvalid length series then
        Nothing

    else if length == 0 then
        Just 1

    else
        let
            product =
                series
                    |> String.split ""
                    |> List.map toInt
                    |> chunkEvery length
                    |> List.foldl compareProducts 0
        in
        Just product



-- PRIVATE


isInvalid : Int -> String -> Bool
isInvalid length series =
    let
        isSpanTooLarge : Int -> String -> Bool
        isSpanTooLarge length_ series_ =
            length_ > String.length series_

        isNonZeroLengthOnEmptyString : Int -> String -> Bool
        isNonZeroLengthOnEmptyString length_ series_ =
            length_ /= 0 && series_ == ""

        hasInvalidCharacterInDigits : String -> Bool
        hasInvalidCharacterInDigits series_ =
            String.any (not << Char.isDigit) series_

        hasNegativeLength : Int -> Bool
        hasNegativeLength length_ =
            length_ < 0
    in
    isSpanTooLarge length series
        || isNonZeroLengthOnEmptyString length series
        || hasInvalidCharacterInDigits series
        || hasNegativeLength length


compareProducts : List Int -> Int -> Int
compareProducts subSeries acc =
    let
        product =
            List.foldl (*) 1 subSeries
    in
    if product > acc then
        product

    else
        acc


toInt : String -> Int
toInt string =
    string
        |> String.toInt
        |> Maybe.withDefault 0


chunkEvery : Int -> List Int -> List (List Int)
chunkEvery size list =
    if size == List.length list then
        [ list ]

    else
        let
            chunk =
                List.take size list

            step =
                1

            tail =
                List.drop step list
        in
        chunk :: chunkEvery size tail
