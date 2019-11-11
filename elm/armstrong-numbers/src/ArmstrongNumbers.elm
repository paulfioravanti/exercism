module ArmstrongNumbers exposing (isArmstrongNumber)


isArmstrongNumber : Int -> Bool
isArmstrongNumber nb =
    let
        parseInt string =
            string
                |> String.toInt
                |> Maybe.withDefault 0

        sumOfPowers =
            nb
                |> String.fromInt
                |> String.split ""
                |> List.map parseInt
                |> sumPowers
    in
    nb == sumOfPowers



-- PRIVATE


sumPowers : List Int -> Int
sumPowers digits =
    let
        length =
            List.length digits
    in
    List.foldl (power length) 0 digits


power : Int -> Int -> Int -> Int
power length digit acc =
    acc + digit ^ length
