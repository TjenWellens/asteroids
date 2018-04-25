module Views.SpaceShuttle exposing (drawSpaceShuttle)

import Data.Momentum exposing (Heading)
import Data.Position exposing (Position)
import Data.SpaceShuttle exposing (SpaceShuttle)
import Svg exposing (..)
import Svg.Attributes exposing (..)

drawSpaceShuttle: SpaceShuttle -> List (Svg msg)
drawSpaceShuttle spaceShuttle =
    [ polygon [ points (toPoints spaceShuttle), stroke "#aaaaaa" ] [] ]

toPoints: SpaceShuttle -> String
toPoints spaceShuttle =
    [ Position  0 -6
    , Position -3  3
    , Position  3  3
    ]
        |> List.map (rotate spaceShuttle.position spaceShuttle.acceleration.heading)
        |> List.map positionToPoint
        |> List.foldl concatWithSpace ""

concatWithSpace: String -> String -> String
concatWithSpace a b = a ++ " " ++ b

positionToPoint: Position -> String
positionToPoint position =
    ((toString position.x) ++ "," ++ (toString position.y))

rotate: Position -> Heading -> Position -> Position
rotate center heading position =
    let
        angle = (getAngleFromHeading heading) + (turns 0.25)

        x = position.x
        y = position.y

--      C11 = A11 B11 + A12 B21
        rotate_x= cos angle * x - sin angle * y

--      C21 = A21 B11 + A22 B21
        rotate_y= sin angle * x + cos angle * y

        newx = rotate_x + center.x
        newy = rotate_y + center.y
    in
        Position newx newy

getAngleFromHeading: Heading -> Float
getAngleFromHeading heading =
    atan2 (toFloat heading.dy) (toFloat heading.dx)