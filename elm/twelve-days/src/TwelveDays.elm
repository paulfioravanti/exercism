module TwelveDays exposing (recite)

import Array
import Dict exposing (Dict)


recite : Int -> Int -> List String
recite start stop =
    List.range start stop
        |> List.map verse



-- PRIVATE


verse : Int -> String
verse number =
    let
        index =
            number - 1

        ordinal =
            valueAtIndex index ordinals

        declarationOfReceipt =
            "On the "
                ++ ordinal
                ++ " day of Christmas my true love gave to me: "

        amount =
            cardinalFromOrdinal ordinal

        currentGift =
            valueAtIndex index gifts

        extraGifts =
            calculateExtraGifts index
    in
    declarationOfReceipt ++ amount ++ currentGift ++ extraGifts ++ "."


valueAtIndex : Int -> List String -> String
valueAtIndex index list =
    list
        |> Array.fromList
        |> Array.get index
        |> Maybe.withDefault "first"


cardinalFromOrdinal : String -> String
cardinalFromOrdinal ordinal =
    let
        removeOrdinalEnding ord =
            ord
                |> String.replace "th" ""
    in
    cardinals
        |> Dict.get ordinal
        |> Maybe.withDefault (removeOrdinalEnding ordinal)


calculateExtraGifts : Int -> String
calculateExtraGifts index =
    let
        giftsForEachDayOfChristmas =
            List.map2 Tuple.pair ordinals gifts
    in
    giftsForEachDayOfChristmas
        |> List.take index
        |> List.foldl addGift []
        |> String.join ""


addGift : ( String, String ) -> List String -> List String
addGift ( ordinal, gift ) acc =
    let
        amount =
            cardinalFromOrdinal ordinal
    in
    case acc of
        [] ->
            [ ", and " ++ amount ++ gift ]

        _ ->
            (", " ++ amount ++ gift) :: acc


cardinals : Dict String String
cardinals =
    Dict.fromList
        [ ( "first", "a" )
        , ( "second", "two" )
        , ( "third", "three" )
        , ( "fifth", "five" )
        , ( "eighth", "eight" )
        , ( "ninth", "nine" )
        , ( "twelfth", "twelve" )
        ]


ordinals : List String
ordinals =
    [ "first"
    , "second"
    , "third"
    , "fourth"
    , "fifth"
    , "sixth"
    , "seventh"
    , "eighth"
    , "ninth"
    , "tenth"
    , "eleventh"
    , "twelfth"
    ]


gifts : List String
gifts =
    [ " Partridge in a Pear Tree"
    , " Turtle Doves"
    , " French Hens"
    , " Calling Birds"
    , " Gold Rings"
    , " Geese-a-Laying"
    , " Swans-a-Swimming"
    , " Maids-a-Milking"
    , " Ladies Dancing"
    , " Lords-a-Leaping"
    , " Pipers Piping"
    , " Drummers Drumming"
    ]
