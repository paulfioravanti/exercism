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
    if not validSides then
        Err "Invalid lengths"

    else if not validInequality then
        Err "Violates inequality"

    else
        sides
            |> Set.fromList
            |> Set.size
            |> determineTriangleType



-- PRIVATE


allSidesPositive : List number -> Bool
allSidesPositive sides =
    let
        isPositive num =
            num > 0
    in
    List.all isPositive sides


triangleInequality : List number -> Bool
triangleInequality sides =
    let
        ( head, tail ) =
            case List.reverse sides of
                h :: t ->
                    ( h, t )

                [] ->
                    ( 0, [] )
    in
    List.sum tail > head


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
