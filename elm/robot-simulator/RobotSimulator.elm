module RobotSimulator exposing
    ( Bearing(..)
    , Robot
    , advance
    , defaultRobot
    , simulate
    , turnLeft
    , turnRight
    )


type Bearing
    = North
    | East
    | South
    | West


type alias Robot =
    { bearing : Bearing
    , coordinates :
        { x : Int
        , y : Int
        }
    }


defaultRobot : Robot
defaultRobot =
    { bearing = North
    , coordinates = { x = 0, y = 0 }
    }


turnRight : Robot -> Robot
turnRight robot =
    bearingsList
        |> leftRotate 1
        |> turn robot


turnLeft : Robot -> Robot
turnLeft robot =
    bearingsList
        |> rightRotate 1
        |> turn robot


advance : Robot -> Robot
advance robot =
    let
        { x, y } =
            robot.coordinates

        newCoordinates =
            case robot.bearing of
                North ->
                    { x = x, y = y + 1 }

                East ->
                    { x = x + 1, y = y }

                South ->
                    { x = x, y = y - 1 }

                West ->
                    { x = x - 1, y = y }
    in
    { robot | coordinates = newCoordinates }


simulate : String -> Robot -> Robot
simulate directions robot =
    directions
        |> String.toList
        |> List.foldl performDirection robot



-- PRIVATE


bearingsList : List Bearing
bearingsList =
    [ North, East, South, West ]


leftRotate : Int -> List Bearing -> List Bearing
leftRotate count bearings =
    case ( count, bearings ) of
        ( _, [] ) ->
            []

        ( 0, _ ) ->
            bearings

        ( _, head :: tail ) ->
            leftRotate (count - 1) (tail ++ [ head ])


rightRotate : Int -> List Bearing -> List Bearing
rightRotate count bearings =
    bearings
        |> List.reverse
        |> leftRotate count
        |> List.reverse


turn : Robot -> List Bearing -> Robot
turn robot newBearings =
    let
        index =
            bearingsList
                |> List.indexedMap Tuple.pair
                |> List.filter (\( _, bearing ) -> bearing == robot.bearing)
                |> List.head
                |> Maybe.withDefault ( 0, robot.bearing )
                |> Tuple.first

        newBearing =
            newBearings
                |> List.drop index
                |> List.head
                |> Maybe.withDefault robot.bearing
    in
    { robot | bearing = newBearing }


performDirection : Char -> Robot -> Robot
performDirection instruction robot =
    case instruction of
        'L' ->
            turnLeft robot

        'R' ->
            turnRight robot

        'A' ->
            advance robot

        _ ->
            robot
