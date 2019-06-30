module Strain exposing (discard, keep)


keep : (a -> Bool) -> List a -> List a
keep predicate list =
    case list of
        head :: tail ->
            if predicate head then
                head :: keep predicate tail

            else
                keep predicate tail

        [] ->
            []


discard : (a -> Bool) -> List a -> List a
discard predicate list =
    list
        |> keep (\element -> not (predicate element))
