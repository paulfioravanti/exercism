module TwoFer exposing (twoFer)


twoFer : Maybe String -> String
twoFer name =
    let
        message string =
            "One for " ++ string ++ ", one for me."
    in
    case name of
        Just string ->
            message string

        Nothing ->
            message "you"
