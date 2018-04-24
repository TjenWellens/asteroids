module Data.SpaceShuttle exposing (..)

import Data.Bullet exposing (Bullet)
import Data.Position exposing (Heading, Position, Velocity)
import Data.Universe exposing (Universe, reappear)

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

fire: SpaceShuttle -> Bullet
fire spaceShuttle =
    let
        position = gun spaceShuttle
        heading = spaceShuttle.heading
    in
        Bullet position heading 2 10

move: SpaceShuttle -> SpaceShuttle
move spaceShuttle =
    let
        x = spaceShuttle.position.x + toFloat spaceShuttle.heading.dx * toFloat spaceShuttle.speed
        y = spaceShuttle.position.y + toFloat spaceShuttle.heading.dy * toFloat spaceShuttle.speed
        newPosition = Position x y
    in
        {spaceShuttle|position = newPosition}

thrust: SpaceShuttle -> SpaceShuttle
thrust spaceShuttle =
    {spaceShuttle|speed = spaceShuttle.speed + 1}