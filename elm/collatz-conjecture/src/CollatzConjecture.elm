module CollatzConjecture exposing (collatz)


collatz : Int -> Result String Int
collatz start =
    if start > 0 then
        Ok (calculate 0 start)

    else
        Err "Only positive numbers are allowed"



-- PRIVATE


calculate : Int -> Int -> Int
calculate steps int =
    if isTerminatingNumber int then
        steps

    else if isEven int then
        int
            |> nDivTwo
            |> calculate (steps + 1)

    else
        int
            |> threeNPlusOne
            |> calculate (steps + 1)


isTerminatingNumber : Int -> Bool
isTerminatingNumber int =
    int == 1


isEven : Int -> Bool
isEven int =
    modBy 2 int == 0


nDivTwo : Int -> Int
nDivTwo n =
    n // 2


threeNPlusOne : Int -> Int
threeNPlusOne n =
    3 * n + 1
