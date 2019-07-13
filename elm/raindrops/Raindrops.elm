module Raindrops exposing (raindrops)


raindrops : Int -> String
raindrops number =
    let
        conversions =
            [ ( 3, "Pling" )
            , ( 5, "Plang" )
            , ( 7, "Plong" )
            ]

        conversion =
            conversions
                |> List.foldl (addRaindrop number) ""
    in
    case conversion of
        "" ->
            String.fromInt number

        _ ->
            conversion



-- PRIVATE


addRaindrop : Int -> ( Int, String ) -> String -> String
addRaindrop number ( factor, raindrop ) acc =
    case modBy factor number of
        0 ->
            acc ++ raindrop

        _ ->
            acc
