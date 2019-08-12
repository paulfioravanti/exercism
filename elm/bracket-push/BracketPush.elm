module BracketPush exposing (isPaired)

import Dict exposing (Dict)


isPaired : String -> Bool
isPaired input =
    input
        |> String.foldl checkPair []
        |> List.isEmpty



-- PRIVATE


checkPair : Char -> List Char -> List Char
checkPair char acc =
    case Dict.get char brackets of
        Just closingBracket ->
            closingBracket :: acc

        Nothing ->
            if isCloseableBracket char acc then
                List.drop 1 acc

            else if isTypeOfClosingBracket char then
                -- unopened closing bracket
                char :: acc

            else
                acc


isCloseableBracket : Char -> List Char -> Bool
isCloseableBracket char acc =
    List.head acc == Just char


isTypeOfClosingBracket : Char -> Bool
isTypeOfClosingBracket char =
    brackets
        |> Dict.values
        |> List.member char


brackets : Dict Char Char
brackets =
    Dict.fromList
        [ ( '[', ']' )
        , ( '{', '}' )
        , ( '(', ')' )
        ]
