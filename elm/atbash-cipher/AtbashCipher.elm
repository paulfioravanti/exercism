module AtbashCipher exposing (decode, encode)


encode : String -> String
encode plain =
    let
        groupSize =
            5
    in
    plain
        |> String.toLower
        |> toTransposedString
        |> chunkEvery groupSize


decode : String -> String
decode cipher =
    toTransposedString cipher



-- PRIVATE


toTransposedString : String -> String
toTransposedString string =
    string
        |> String.filter Char.isAlphaNum
        |> String.map transpose


transpose : Char -> Char
transpose char =
    if Char.isAlpha char then
        let
            alphabetBounds =
                Char.toCode 'a' + Char.toCode 'z'
        in
        Char.fromCode (alphabetBounds - Char.toCode char)

    else
        char


chunkEvery : Int -> String -> String
chunkEvery size string =
    if size >= String.length string then
        string

    else
        let
            chunk =
                String.left size string

            tail =
                String.dropLeft size string
        in
        chunk ++ " " ++ chunkEvery size tail
