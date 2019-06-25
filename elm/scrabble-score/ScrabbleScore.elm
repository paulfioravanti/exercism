module ScrabbleScore exposing (scoreWord)


scoreWord : String -> Int
scoreWord x =
    x
        |> String.toLower
        |> String.toList
        |> List.foldl addScoreForLetter 0



-- PRIVATE


addScoreForLetter : Char -> Int -> Int
addScoreForLetter char acc =
    if onePoint char then
        acc + 1

    else if twoPoints char then
        acc + 2

    else if threePoints char then
        acc + 3

    else if fourPoints char then
        acc + 4

    else if fivePoints char then
        acc + 5

    else if eightPoints char then
        acc + 8

    else if tenPoints char then
        acc + 10

    else
        acc


onePoint : Char -> Bool
onePoint char =
    let
        onePointCharacters =
            [ 'a', 'e', 'i', 'o', 'u', 'l', 'n', 'r', 's', 't' ]
    in
    List.member char onePointCharacters


twoPoints : Char -> Bool
twoPoints char =
    let
        twoPointCharacters =
            [ 'd', 'g' ]
    in
    List.member char twoPointCharacters


threePoints : Char -> Bool
threePoints char =
    let
        threePointCharacters =
            [ 'b', 'c', 'm', 'p' ]
    in
    List.member char threePointCharacters


fourPoints : Char -> Bool
fourPoints char =
    let
        fourPointCharacters =
            [ 'f', 'h', 'v', 'w', 'y' ]
    in
    List.member char fourPointCharacters


fivePoints : Char -> Bool
fivePoints char =
    char == 'k'


eightPoints : Char -> Bool
eightPoints char =
    let
        eightPointCharacters =
            [ 'j', 'x' ]
    in
    List.member char eightPointCharacters


tenPoints : Char -> Bool
tenPoints char =
    let
        tenPointCharacters =
            [ 'q', 'z' ]
    in
    List.member char tenPointCharacters
