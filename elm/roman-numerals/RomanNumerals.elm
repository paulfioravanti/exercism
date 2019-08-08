module RomanNumerals exposing (toRoman)


toRoman : Int -> String
toRoman number =
    romanNumerals
        |> List.foldl appendRomanNumeral ( "", number )
        |> Tuple.first



-- PRIVATE


appendRomanNumeral : ( String, Int ) -> ( String, Int ) -> ( String, Int )
appendRomanNumeral ( roman, decimal ) ( numeral, number ) =
    let
        quotient =
            number // decimal

        remainder =
            remainderBy decimal number

        string =
            numeral ++ String.repeat quotient roman
    in
    ( string, remainder )


romanNumerals : List ( String, Int )
romanNumerals =
    [ ( "M", 1000 )
    , ( "CM", 900 )
    , ( "D", 500 )
    , ( "CD", 400 )
    , ( "C", 100 )
    , ( "XC", 90 )
    , ( "L", 50 )
    , ( "XL", 40 )
    , ( "X", 10 )
    , ( "IX", 9 )
    , ( "V", 5 )
    , ( "IV", 4 )
    , ( "I", 1 )
    ]
