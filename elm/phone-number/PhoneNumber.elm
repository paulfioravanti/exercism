module PhoneNumber exposing (getNumber)

import Parser exposing ((|.), (|=), Parser)


{-| North American Numbering Plan (NANP)
-}
type alias NANPNumber =
    { areaCode : String
    , exchangeCode : String
    , subscriberNumber : String
    }


getNumber : String -> Maybe String
getNumber phoneNumber =
    let
        nanpNumber =
            Parser.run nanpNumberParser phoneNumber
    in
    case nanpNumber of
        Ok { areaCode, exchangeCode, subscriberNumber } ->
            Just (areaCode ++ exchangeCode ++ subscriberNumber)

        Err _ ->
            Nothing



-- PRIVATE


nanpNumberParser : Parser NANPNumber
nanpNumberParser =
    Parser.succeed NANPNumber
        |. countryCode
        |. whitespace
        |. leftParenthesis
        |= nxxComponent
        |. rightParenthesis
        |. divider
        |= nxxComponent
        |. divider
        |= xxxxComponent
        |. whitespace
        |. Parser.end


countryCode : Parser ()
countryCode =
    Parser.chompWhile (\char -> char == '+' || char == '1')


whitespace : Parser ()
whitespace =
    Parser.chompWhile (\char -> char == ' ')


leftParenthesis : Parser ()
leftParenthesis =
    Parser.chompWhile (\char -> char == '(')


{-| NXX component of a NANP number covers the 3-digit pattern for the area code
and exchange code, where N is a number between 2-9, and Xs are any digit.
REF: <https://en.wikipedia.org/wiki/North_American_Numbering_Plan#Modern_plan>
-}
nxxComponent : Parser String
nxxComponent =
    let
        between2and9 char =
            Char.isDigit char && char > '1' && char <= '9'
    in
    Parser.getChompedString
        (Parser.succeed ()
            |. Parser.chompIf between2and9
            |. Parser.chompIf Char.isDigit
            |. Parser.chompIf Char.isDigit
        )


rightParenthesis : Parser ()
rightParenthesis =
    Parser.chompWhile (\char -> char == ')')


divider : Parser ()
divider =
    let
        dividerCharacter char =
            char == ' ' || char == '.' || char == '-'
    in
    Parser.chompWhile dividerCharacter


{-| XXXX component of a NANP number covers the 4-digit pattern for a subscriber
number.
REF: <https://en.wikipedia.org/wiki/North_American_Numbering_Plan#Modern_plan>
-}
xxxxComponent : Parser String
xxxxComponent =
    Parser.getChompedString
        (Parser.succeed ()
            |. Parser.chompIf Char.isDigit
            |. Parser.chompIf Char.isDigit
            |. Parser.chompIf Char.isDigit
            |. Parser.chompIf Char.isDigit
        )
