module Hamming exposing (distance)


distance : String -> String -> Result String Int
distance left right =
    if left == right then
        Ok 0

    else if sameLength left right then
        let
            hammingDistance =
                right
                    |> String.toList
                    |> zip (String.toList left)
                    |> List.foldl incrementDifference 0
        in
        Ok hammingDistance

    else
        Err "left and right strands must be of equal length"



-- PRIVATE


sameLength : String -> String -> Bool
sameLength left right =
    String.length left == String.length right


zip : List Char -> List Char -> List ( Char, Char )
zip leftList rightList =
    List.map2 Tuple.pair leftList rightList


incrementDifference : ( Char, Char ) -> Int -> Int
incrementDifference ( leftNucleotide, rightNucleotide ) acc =
    if leftNucleotide /= rightNucleotide then
        acc + 1

    else
        acc
