module Bob exposing (hey)


hey : String -> String
hey input =
    let
        remark =
            String.trim input

        isSilence =
            remark == ""
    in
    if isSilence then
        "Fine. Be that way!"

    else
        respondToVerbalRemark remark



-- PRIVATE


respondToVerbalRemark : String -> String
respondToVerbalRemark remark =
    let
        isQuestion =
            String.endsWith "?" remark

        hasLetters =
            String.any Char.isAlpha remark

        isShouting =
            hasLetters && String.toUpper remark == remark
    in
    case ( isQuestion, isShouting ) of
        ( True, True ) ->
            "Calm down, I know what I'm doing!"

        ( True, False ) ->
            "Sure."

        ( False, True ) ->
            "Whoa, chill out!"

        ( False, False ) ->
            "Whatever."
