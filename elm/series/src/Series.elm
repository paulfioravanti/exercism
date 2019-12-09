module Series exposing (slices)


slices : Int -> String -> Result String (List (List Int))
slices size input =
    let
        length =
            String.length input
    in
    if size == 0 then
        Err "slice length cannot be zero"

    else if size < 0 then
        Err "slice length cannot be negative"

    else if length == 0 then
        Err "series cannot be empty"

    else if size > length then
        Err "slice length cannot be greater than series length"

    else
        input
            |> toIntList
            |> chunkEvery size
            |> Ok



-- PRIVATE


toIntList : String -> List Int
toIntList input =
    let
        toInt string =
            string
                |> String.toInt
                |> Maybe.withDefault 0
    in
    input
        |> String.split ""
        |> List.map toInt


chunkEvery : Int -> List Int -> List (List Int)
chunkEvery size inputList =
    if size == List.length inputList then
        [ inputList ]

    else
        let
            chunk =
                List.take size inputList

            step =
                1

            tail =
                List.drop step inputList
        in
        chunk :: chunkEvery size tail
