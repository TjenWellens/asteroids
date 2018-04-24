module Data.SpaceShuttle exposing (..)

import Data.Position exposing (Heading, Position, Velocity)

type alias SpaceShuttle =
    { position: Position
    , heading: Heading
    , speed: Velocity
    }

gun: SpaceShuttle -> Position
gun spaceShuttle =
    let
        gunDistance = 7.0
        x = spaceShuttle.position.x + toFloat spaceShuttle.heading.dx * gunDistance
        y = spaceShuttle.position.y + toFloat spaceShuttle.heading.dy * gunDistance
    in
        Position x y

rotate: SpaceShuttle -> Heading -> SpaceShuttle
rotate spaceShuttle heading =
    {spaceShuttle|heading=heading}