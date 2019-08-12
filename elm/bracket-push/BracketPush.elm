module BracketPush exposing (isPaired)


isPaired : String -> Bool
isPaired input =
    let
        result =
            input
                |> String.toList
                |> List.foldl checkPair (Ok [])
    in
    case result of
        Ok list ->
            List.isEmpty list

        Err _ ->
            False



-- PRIVATE


checkPair :
    Char
    -> Result (List Char) (List Char)
    -> Result (List Char) (List Char)
checkPair char acc =
    if List.member char openingBrackets then
        acc
            |> Result.map ((::) char)

    else if List.member char closingBrackets then
        let
            rawAcc =
                Result.withDefault [] acc
        in
        if hasMatchingOpeningBracket char rawAcc then
            rawAcc
                |> List.tail
                |> Result.fromMaybe rawAcc

        else
            Err rawAcc

    else
        acc


hasMatchingOpeningBracket : Char -> List Char -> Bool
hasMatchingOpeningBracket char acc =
    let
        closingBracketIndex =
            closingBrackets
                |> List.indexedMap Tuple.pair
                |> List.filter (\( _, bracket ) -> char == bracket)
                |> List.head
                |> Maybe.withDefault ( 0, ' ' )
                |> Tuple.first

        openingBracket =
            openingBrackets
                |> List.indexedMap Tuple.pair
                |> List.filter (\( index, _ ) -> index == closingBracketIndex)
                |> List.head
                |> Maybe.withDefault ( 0, ' ' )
                |> Tuple.second

        accHead =
            acc
                |> List.head
                |> Maybe.withDefault ' '
    in
    accHead == openingBracket


openingBrackets : List Char
openingBrackets =
    [ '(', '[', '{' ]


closingBrackets : List Char
closingBrackets =
    [ ')', ']', '}' ]
