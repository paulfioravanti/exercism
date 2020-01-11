module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year =
  isLeap year && (isNonCenturialYear year || isLeapCycleYear year)

-- PRIVATE

isLeap :: Integer -> Bool
isLeap year = mod year leapYear == 0

isNonCenturialYear :: Integer -> Bool
isNonCenturialYear year = mod year centurialYear /= 0

isLeapCycleYear :: Integer -> Bool
isLeapCycleYear year =
  let leapCycleLength = leapYear * centurialYear
  in mod year leapCycleLength == 0

leapYear :: Integer
leapYear = 4

centurialYear :: Integer
centurialYear = 100
