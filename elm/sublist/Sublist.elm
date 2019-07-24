module Sublist exposing (ListComparison(..), sublist)


type ListComparison
    = Equal
    | Superlist
    | Sublist
    | Unequal


sublist : List a -> List a -> ListComparison
sublist alist blist =
    if alist == blist then
        Equal

    else if (blist |> isShorter alist) && (alist |> isContainedIn blist) then
        Sublist

    else if (alist |> isShorter blist) && (blist |> isContainedIn alist) then
        Superlist

    else
        Unequal



-- PRIVATE


isContainedIn : List a -> List a -> Bool
isContainedIn xlist ylist =
    let
        xsublist =
            xlist
                |> List.take (List.length ylist)
    in
    if xsublist == ylist then
        True

    else
        case xlist of
            [] ->
                False

            xhead :: xtail ->
                isContainedIn xtail ylist


isShorter : List a -> List a -> Bool
isShorter xlist ylist =
    List.length xlist < List.length ylist
