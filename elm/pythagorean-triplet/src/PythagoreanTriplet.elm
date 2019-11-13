module PythagoreanTriplet exposing (triplets)


type alias Triplet =
    ( Int, Int, Int )


triplets : Int -> List Triplet
triplets n =
    let
        max =
            n - 2

        aRange =
            List.range 1 max
    in
    aRange
        |> List.map (generate n)
        |> List.concat



-- PRIVATE


generate : Int -> Int -> List Triplet
generate n a =
    let
        max =
            n - 1

        bRange =
            List.range a max
    in
    bRange
        |> List.map (triplet n a)
        |> List.filter isPythagorean


triplet : Int -> Int -> Int -> Triplet
triplet n a b =
    let
        c =
            n - a - b
    in
    ( a, b, c )


isPythagorean : Triplet -> Bool
isPythagorean ( a, b, c ) =
    a ^ 2 + b ^ 2 == c ^ 2
