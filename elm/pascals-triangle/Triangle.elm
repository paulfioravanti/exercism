module Triangle exposing (rows)


rows : Int -> List (List Int)
rows n =
    {- https://en.wikipedia.org/wiki/Pascal%27s_triangle
       "The rows of Pascal's triangle are conventionally enumerated
       starting with row n = 0 at the top (the 0th row)", so instantly decrement
       the number of rows by 1.
    -}
    List.range 0 (n - 1)
        |> List.map generateRow



-- PRIVATE


generateRow : Int -> List Int
generateRow rowNum =
    List.range 0 rowNum
        |> List.map (binomial rowNum)


binomial : Int -> Int -> Int
binomial rowNum exponent =
    {- https://en.wikipedia.org/wiki/Binomial_theorem
       "n (rowNum) choose k (exponent)" => n!/(n - k)!k!
    -}
    factorial rowNum // (factorial (rowNum - exponent) * factorial exponent)


factorial : Int -> Int
factorial n =
    case n of
        0 ->
            1

        _ ->
            n * factorial (n - 1)
