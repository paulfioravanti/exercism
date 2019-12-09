module Accumulate exposing (accumulate)


accumulate : (a -> b) -> List a -> List b
accumulate func input =
    case input of
        head :: tail ->
            func head :: accumulate func tail

        [] ->
            []
