module Allergies exposing (isAllergicTo, toList)

import Bitwise


isAllergicTo : String -> Int -> Bool
isAllergicTo name score =
    score
        |> toList
        |> List.member name


toList : Int -> List String
toList score =
    allergens
        |> List.indexedMap Tuple.pair
        |> List.filter (isAllergen score)
        |> List.map Tuple.second



-- PRIVATE


allergens : List String
allergens =
    [ "eggs"
    , "peanuts"
    , "shellfish"
    , "strawberries"
    , "tomatoes"
    , "chocolate"
    , "pollen"
    , "cats"
    ]


isAllergen : Int -> ( Int, String ) -> Bool
isAllergen score ( index, _ ) =
    let
        indexAllergenScore =
            index
                |> allergenScore
                |> Bitwise.and score
    in
    indexAllergenScore > 0


allergenScore : Int -> Int
allergenScore index =
    1 |> Bitwise.shiftLeftBy index
