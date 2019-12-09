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

    else if alist |> isContainedIn blist then
        Sublist

    else if blist |> isContainedIn alist then
        Superlist

    else
        Unequal



-- PRIVATE


isContainedIn : List a -> List a -> Bool
isContainedIn xlist ylist =
    if ylist |> isShorter xlist then
        False

    else if ylist |> isSublist xlist then
        True

    else
        let
            xtail =
                List.drop 1 xlist
        in
        isContainedIn xtail ylist


isShorter : List a -> List a -> Bool
isShorter xlist ylist =
    List.length xlist < List.length ylist


isSublist : List a -> List a -> Bool
isSublist xlist ylist =
    let
        ylength =
            List.length ylist
    in
    xlist
        |> List.take ylength
        |> (==) ylist
