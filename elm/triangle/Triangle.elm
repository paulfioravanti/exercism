module Triangle exposing (Triangle(..), triangleKind)

import Set


type Triangle
    = Equilateral
    | Isosceles
    | Scalene


triangleKind : number -> number -> number -> Result String Triangle
triangleKind x y z =
    let
        sides =
            List.sort [ x, y, z ]

        validSides =
            allSidesPositive sides

        validInequality =
            triangleInequality sides
    in
    case ( validSides, validInequality ) of
        ( Err err, _ ) ->
            Err err

        ( _, Err err ) ->
            Err err

        _ ->
            sides
                |> Set.fromList
                |> Set.size
                |> determineTriangleType



-- PRIVATE


allSidesPositive : List number -> Result String Bool
allSidesPositive sides =
    let
        isPositive num =
            num > 0
    in
    if List.all isPositive sides then
        Ok True

    else
        Err "Invalid lengths"


triangleInequality : List number -> Result String Bool
triangleInequality sides =
    let
        ( head, tail ) =
            case List.reverse sides of
                h :: t ->
                    ( h, t )

                [] ->
                    ( 0, [] )
    in
    if List.sum tail > head then
        Ok True

    else
        Err "Violates inequality"


determineTriangleType : Int -> Result String Triangle
determineTriangleType numUniqueSides =
    case numUniqueSides of
        1 ->
            Ok Equilateral

        2 ->
            Ok Isosceles

        3 ->
            Ok Scalene

        _ ->
            Err "Not a triangle"
