module Grains exposing (square)


square : Int -> Maybe Int
square n =
    if validChessboardSquare n then
        Just (calculateSquare n)

    else
        Nothing



-- PRIVATE


validChessboardSquare : Int -> Bool
validChessboardSquare n =
    List.range 1 64
        |> List.member n


calculateSquare : Int -> Int
calculateSquare n =
    let
        base =
            2

        offset =
            1
    in
    base ^ (n - offset)
