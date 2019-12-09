module TwoFer exposing (twoFer)


twoFer : Maybe String -> String
twoFer name =
    let
        companion =
            name
                |> Maybe.withDefault "you"
    in
    "One for " ++ companion ++ ", one for me."
