module SumOfMultiples exposing (sumOfMultiples)


sumOfMultiples : List Int -> Int -> Int
sumOfMultiples multiples limit =
    limit
        |> multipleRange
        |> List.filter (areMultiples multiples)
        |> List.sum



-- PRIVATE


multipleRange : Int -> List Int
multipleRange limit =
    List.range 1 (limit - 1)


areMultiples : List Int -> Int -> Bool
areMultiples multiples rangeNumber =
    multiples
        |> List.any (isMultiple rangeNumber)


isMultiple : Int -> Int -> Bool
isMultiple rangeNumber multiple =
    rangeNumber
        |> modBy multiple
        |> (==) 0
