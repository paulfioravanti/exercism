module AtbashCipher exposing (decode, encode)


encode : String -> String
encode plain =
    let
        groupSize =
            5
    in
    plain
        |> String.toLower
        |> toTransposedList
        |> chunkEvery groupSize
        |> List.map joinChars
        |> String.join " "


decode : String -> String
decode cipher =
    cipher
        |> toTransposedList
        |> joinChars



-- PRIVATE


toTransposedList : String -> List Char
toTransposedList string =
    string
        |> String.toList
        |> List.foldr transpose []


joinChars : List Char -> String
joinChars list =
    list
        |> List.map String.fromChar
        |> String.join ""


transpose : Char -> List Char -> List Char
transpose char acc =
    if Char.isAlpha char then
        let
            alphabetBounds =
                Char.toCode 'a' + Char.toCode 'z'

            shift character =
                (alphabetBounds - Char.toCode character)
                    |> Char.fromCode
        in
        shift char :: acc

    else if Char.isDigit char then
        char :: acc

    else
        acc


chunkEvery : Int -> List Char -> List (List Char)
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
