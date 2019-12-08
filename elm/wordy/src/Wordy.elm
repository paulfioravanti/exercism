module Wordy exposing (answer)

import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser)


type alias MathQuestion =
    { initialValue : Int
    , operations : String
    }


type alias Operator =
    Int -> Int -> Int


answer : String -> Maybe Int
answer problem =
    let
        parsedMathQuestion =
            Parser.run mathQuestionParser problem
    in
    case parsedMathQuestion of
        Ok ({ initialValue, operations } as mathQuestion) ->
            case operations of
                "" ->
                    Just initialValue

                _ ->
                    performMathOperations mathQuestion

        Err _ ->
            Nothing



-- PRIVATE


performMathOperations : MathQuestion -> Maybe Int
performMathOperations { initialValue, operations } =
    let
        performOperation : Maybe ( Operator, Int ) -> Maybe Int -> Maybe Int
        performOperation operatorOperand acc =
            case ( operatorOperand, acc ) of
                ( Just ( operator, operand2 ), Just operand1 ) ->
                    Just (operator operand1 operand2)

                _ ->
                    Nothing
    in
    operations
        |> parseOperations
        |> List.foldl performOperation (Just initialValue)


parseOperations : String -> List (Maybe ( Operator, Int ))
parseOperations stringOperationList =
    stringOperationList
        |> String.replace "by " ""
        |> String.split " "
        |> chunkEvery 2
        |> List.map operatorOperandPair


chunkEvery : Int -> List String -> List (List String)
chunkEvery size list =
    if size >= List.length list then
        [ list ]

    else
        let
            chunk =
                List.take size list

            tail =
                List.drop size list
        in
        chunk :: chunkEvery size tail


operatorOperandPair : List String -> Maybe ( Operator, Int )
operatorOperandPair list =
    let
        operatorDict : Dict String Operator
        operatorDict =
            Dict.fromList
                [ ( "plus", (+) )
                , ( "minus", (-) )
                , ( "multiplied", (*) )
                , ( "divided", (//) )
                ]
    in
    case list of
        [ operatorName, stringOperand ] ->
            let
                maybeOperator =
                    Dict.get operatorName operatorDict

                maybeOperand =
                    String.toInt stringOperand
            in
            case ( maybeOperator, maybeOperand ) of
                ( Just operator, Just operand ) ->
                    Just ( operator, operand )

                _ ->
                    Nothing

        _ ->
            Nothing



-- PARSER


mathQuestionParser : Parser MathQuestion
mathQuestionParser =
    Parser.succeed MathQuestion
        |. Parser.token "What is "
        |= signedInteger
        |. whitespace
        |= operationSet
        |. Parser.token "?"
        |. Parser.end


signedInteger : Parser Int
signedInteger =
    Parser.oneOf
        [ Parser.succeed negate
            |. Parser.symbol "-"
            |= Parser.int
        , Parser.int
        ]


whitespace : Parser ()
whitespace =
    Parser.chompWhile (\char -> char == ' ')


operationSet : Parser String
operationSet =
    Parser.getChompedString
        (Parser.succeed ()
            |. Parser.chompUntil "?"
        )
