module AllYourBase exposing (rebase)


rebase : Int -> List Int -> Int -> Maybe (List Int)
rebase inBase digits outBase =
    case digits of
        [] ->
            Nothing

        0 :: tail ->
            rebase inBase tail outBase

        _ ->
            if
                hasInvalidBases inBase outBase
                    || containsInvalidDigits inBase digits
            then
                Nothing

            else
                let
                    outBaseDigits =
                        digits
                            |> sumInput inBase
                            |> convertToOutBaseDigits outBase []
                in
                Just outBaseDigits



-- PRIVATE


hasInvalidBases : Int -> Int -> Bool
hasInvalidBases inBase outBase =
    let
        minimumBase =
            2
    in
    inBase < minimumBase || outBase < minimumBase


containsInvalidDigits : Int -> List Int -> Bool
containsInvalidDigits inBase digits =
    let
        isInvalidDigit base digit =
            digit < 0 || digit >= base
    in
    digits
        |> List.any (isInvalidDigit inBase)


sumInput : Int -> List Int -> Int
sumInput inBase digits =
    digits
        |> List.reverse
        |> List.indexedMap Tuple.pair
        |> List.foldl (addPower inBase) 0


addPower : Int -> ( Int, Int ) -> Int -> Int
addPower inBase ( index, digit ) acc =
    acc + digit * inBase ^ index


convertToOutBaseDigits : Int -> List Int -> Int -> List Int
convertToOutBaseDigits outBase digits total =
    let
        remainder =
            modBy outBase total

        outBaseDigits =
            remainder :: digits
    in
    if total < outBase then
        outBaseDigits

    else
        (total // outBase)
            |> convertToOutBaseDigits outBase outBaseDigits
