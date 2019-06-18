module DifferenceOfSquares exposing (difference, squareOfSum, sumOfSquares)


squareOfSum : Int -> Int
squareOfSum n =
    List.range 1 n
        |> List.sum
        |> square


sumOfSquares : Int -> Int
sumOfSquares n =
    List.range 1 n
        |> List.foldl addSquare 0


difference : Int -> Int
difference n =
    squareOfSum n - sumOfSquares n



-- PRIVATE


addSquare : Int -> Int -> Int
addSquare n acc =
    acc + square n


square : Int -> Int
square n =
    n ^ 2
