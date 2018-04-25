module Data.SpaceShuttle exposing (..)

import Data.Bullet exposing (Bullet)
import Data.Momentum exposing (Heading, Speed)
import Data.Position exposing (Acceleration, Position)
import Data.Universe exposing (Universe, reappear)

type alias SpaceShuttle =
    { position: Position
    , heading: Heading
    , speed: Speed
    , acceleration: Acceleration
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
        speed = spaceShuttle.speed + 4
        range = 10
    in
        Bullet position heading speed range

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
    {spaceShuttle|acceleration = 1}

accelerate: SpaceShuttle -> SpaceShuttle
accelerate spaceShuttle =
    { spaceShuttle
        | speed = spaceShuttle.speed + spaceShuttle.acceleration
        , acceleration = 0
    }