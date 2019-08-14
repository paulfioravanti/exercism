module BinarySearch exposing (find)

import Array exposing (Array)


find : Int -> Array Int -> Maybe Int
find target xs =
    if Array.isEmpty xs then
        Nothing

    else
        let
            rightIndex =
                Array.length xs - 1
        in
        search target 0 rightIndex xs



-- PRIVATE


search : Int -> Int -> Int -> Array Int -> Maybe Int
search target left right xs =
    case compare left right of
        {- NOTE: This condition is met when search options have been exhausted,
           and the `left` index will have been incremented, or the `right` index
           decremented, to a number where we would be attempting to perform an
           "out-of-bounds" search on position `left` of a "sub-array" of length
           `right`.
        -}
        GT ->
            Nothing

        _ ->
            let
                middleIndex =
                    (left + right) // 2
            in
            case Array.get middleIndex xs of
                Nothing ->
                    Nothing

                Just middleElement ->
                    case compare middleElement target of
                        GT ->
                            search target left (middleIndex - 1) xs

                        LT ->
                            search target (middleIndex + 1) right xs

                        EQ ->
                            Just middleIndex
