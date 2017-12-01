module Bob exposing (hey)

import Regex exposing (regex)


hey : String -> String
hey message =
    if silence message then
        "Fine. Be that way!"
    else if isShouting message then
        "Whoa, chill out!"
    else if isQuestion message then
        "Sure."
    else
        "Whatever."


silence : String -> Bool
silence message =
    String.trim message == ""


isShouting : String -> Bool
isShouting message =
    not (onlyDigitsAndNonWords message)
        && String.toUpper message
        == message


onlyDigitsAndNonWords : String -> Bool
onlyDigitsAndNonWords message =
    Regex.contains (regex "^([0-9]|[^a-zA-Z])+$") message


isQuestion : String -> Bool
isQuestion message =
    String.endsWith "?" message
