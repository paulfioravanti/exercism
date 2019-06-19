module Gigasecond exposing (add)

import Time


add : Time.Posix -> Time.Posix
add timestamp =
    let
        milliSeconds =
            10 ^ 12
    in
    timestamp
        |> Time.posixToMillis
        |> (+) milliSeconds
        |> Time.millisToPosix
