module Leap exposing (isLeapYear)


isLeapYear : Int -> Bool
isLeapYear year =
    isLeap year && (isNonCenturialYear year || isLeapCycleYear year)



-- PRIVATE


isLeap : Int -> Bool
isLeap year =
    year
        |> modBy leapYear
        |> (==) 0


isNonCenturialYear : Int -> Bool
isNonCenturialYear year =
    year
        |> modBy centurialYear
        |> (==) 0
        |> not


isLeapCycleYear : Int -> Bool
isLeapCycleYear year =
    let
        leapCycleLength =
            leapYear * centurialYear
    in
    year
        |> modBy leapCycleLength
        |> (==) 0


leapYear : Int
leapYear =
    4


centurialYear : Int
centurialYear =
    100
