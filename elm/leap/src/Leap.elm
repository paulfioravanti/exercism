module Leap exposing (isLeapYear)


isLeapYear : Int -> Bool
isLeapYear year =
    isLeap year && (isNonCenturialYear year || isLeapCycleYear year)



-- PRIVATE


isLeap : Int -> Bool
isLeap year =
    modBy leapYear year == 0


isNonCenturialYear : Int -> Bool
isNonCenturialYear year =
    not (modBy centurialYear year == 0)


isLeapCycleYear : Int -> Bool
isLeapCycleYear year =
    let
        leapCycleLength =
            leapYear * centurialYear
    in
    modBy leapCycleLength year == 0


leapYear : Int
leapYear =
    4


centurialYear : Int
centurialYear =
    100
