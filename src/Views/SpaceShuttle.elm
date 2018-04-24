module Views.SpaceShuttle exposing (drawSpaceShuttle)

import Data.Position exposing (Position)
import Data.SpaceShuttle exposing (SpaceShuttle)
import Svg exposing (..)
import Svg.Attributes exposing (..)

drawSpaceShuttle: SpaceShuttle -> List (Svg msg)
drawSpaceShuttle spaceShuttle =
    [ polygon [ points (toPoints spaceShuttle), stroke "#FFFFFF" ] [] ]

toPoints: SpaceShuttle -> String
toPoints spaceShuttle =
    spaceShuttle
        |> toPositions
        |> List.map positionToPoint
        |> List.foldl concatWithSpace ""

concatWithSpace: String -> String -> String
concatWithSpace a b = a ++ " " ++ b

positionToPoint: Position -> String
positionToPoint position =
    ((toString position.x) ++ "," ++ (toString position.y))

toPositions: SpaceShuttle -> List Position
toPositions spaceShuttle =
    let
        p=spaceShuttle.position
    in
        [ translate p  0 -6
        , translate p -3  3
        , translate p  3  3
        ]

translate: Position -> Int -> Int -> Position
translate p dx dy =
    Position (p.x + dx) (p.y + dy)