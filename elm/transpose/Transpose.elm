module Transpose exposing (transpose)


transpose : List String -> List String
transpose lines =
    case lines of
        [] ->
            []

        _ ->
            let
                maxWidth =
                    maxRowWidth lines
            in
            lines
                |> List.map (generateRightPaddedLine maxWidth)
                |> transposeLines
                |> revertPadding



-- PRIVATE


{-| Chosen arbitrarily...
-}
paddingCharacter : Char
paddingCharacter =
    '$'


isPaddingCharacter : Char -> Bool
isPaddingCharacter char =
    char == paddingCharacter


maxRowWidth : List String -> Int
maxRowWidth lines =
    lines
        |> List.map String.length
        |> List.maximum
        |> Maybe.withDefault 0


generateRightPaddedLine : Int -> String -> List String
generateRightPaddedLine maxWidth line =
    line
        |> String.padRight maxWidth paddingCharacter
        |> String.split ""


transposeLines : List (List String) -> List String
transposeLines lines =
    lines
        |> zip []
        |> List.map (String.join "")


zip : List (List String) -> List (List String) -> List (List String)
zip zippedLines lines =
    case lines of
        [] ->
            zippedLines
                |> List.map List.reverse

        head :: tail ->
            case zippedLines of
                [] ->
                    let
                        newZippedLines =
                            List.map List.singleton head
                    in
                    zip newZippedLines tail

                _ ->
                    let
                        newZippedLines =
                            List.map2 (::) head zippedLines
                    in
                    zip newZippedLines tail


revertPadding : List String -> List String
revertPadding lines =
    let
        substitutePaddingCharacterForSpace : Char -> Char
        substitutePaddingCharacterForSpace char =
            if isPaddingCharacter char then
                ' '

            else
                char
    in
    lines
        |> List.map String.reverse
        |> List.map String.toList
        |> List.map (dropWhile isPaddingCharacter)
        |> List.map (List.map substitutePaddingCharacterForSpace)
        |> List.map String.fromList
        |> List.map String.reverse


dropWhile : (a -> Bool) -> List a -> List a
dropWhile predicate list =
    case list of
        [] ->
            []

        head :: tail ->
            if predicate head then
                dropWhile predicate tail

            else
                list
