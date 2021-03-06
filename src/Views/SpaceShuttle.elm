module Views.SpaceShuttle exposing (drawSpaceShuttle)

import Data.Heading as Heading exposing (Heading)
import Data.Position exposing (Position)
import Data.SpaceShuttle as SpaceShuttle exposing (SpaceShuttle, Livelyness(..))
import Svg exposing (..)
import Svg.Attributes exposing (..)

drawSpaceShuttle: SpaceShuttle -> List (Svg msg)
drawSpaceShuttle spaceShuttle =
    let
        color = getColor spaceShuttle
    in
        [ polygon [ points (toPoints spaceShuttle), stroke color ] [] ]

getColor: SpaceShuttle -> String
getColor spaceShuttle =
    case spaceShuttle.livelyness of
        Alive -> "#aaaaaa"
        Dead -> "#ff0000"

toPoints: SpaceShuttle -> String
toPoints spaceShuttle =
    [ Position  0 -6
    , Position -3  3
    , Position  3  3
    ]
        |> List.map (rotate spaceShuttle.position spaceShuttle.aim)
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
        angle = (Heading.toAngle heading) + (turns 0.25)

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
