module Hamming exposing (distance)


distance : String -> String -> Result String Int
distance left right =
    case ( String.uncons left, String.uncons right ) of
        ( Just ( leftHead, leftTail ), Just ( rightHead, rightTail ) ) ->
            let
                hammingDistance =
                    if leftHead == rightHead then
                        0

                    else
                        1
            in
            Result.map ((+) hammingDistance) (distance leftTail rightTail)

        ( Nothing, Nothing ) ->
            Ok 0

        _ ->
            Err "left and right strands must be of equal length"
