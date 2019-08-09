module Acronym exposing (abbreviate)


abbreviate : String -> String
abbreviate phrase =
    phrase
        |> String.replace "-" " "
        |> String.split " "
        |> List.map (String.left 1)
        |> String.join ""
        |> String.toUpper
