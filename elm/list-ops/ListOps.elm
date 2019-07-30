module ListOps exposing
    ( append
    , concat
    , filter
    , foldl
    , foldr
    , length
    , map
    , reverse
    )


length : List a -> Int
length list =
    case list of
        [] ->
            0

        _ :: tail ->
            1 + length tail


reverse : List a -> List a
reverse list =
    case list of
        [] ->
            []

        head :: tail ->
            reverse tail ++ [ head ]


foldl : (a -> b -> b) -> b -> List a -> b
foldl f acc list =
    case list of
        [] ->
            acc

        head :: tail ->
            foldl f (f head acc) tail


foldr : (a -> b -> b) -> b -> List a -> b
foldr f acc list =
    case list of
        [] ->
            acc

        head :: tail ->
            f head (foldr f acc tail)


map : (a -> b) -> List a -> List b
map f list =
    case list of
        [] ->
            []

        head :: tail ->
            f head :: map f tail


filter : (a -> Bool) -> List a -> List a
filter f list =
    case list of
        [] ->
            []

        head :: tail ->
            if f head then
                head :: filter f tail

            else
                filter f tail


append : List a -> List a -> List a
append xs ys =
    case ( xs, ys ) of
        ( [], [] ) ->
            []

        ( xs_, [] ) ->
            xs_

        ( [], ys_ ) ->
            ys_

        ( xs_, ys_ ) ->
            foldr (::) ys_ xs_


concat : List (List a) -> List a
concat list =
    case list of
        [] ->
            []

        head :: tail ->
            foldr (::) (concat tail) head
