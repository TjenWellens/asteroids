module Data.SpaceShuttle exposing (..)

import Data.Bullet exposing (Bullet)
import Data.Momentum exposing (Momentum, Heading, Speed)
import Data.Position exposing (Acceleration, Position)
import Data.Universe exposing (Universe, reappear)

type alias SpaceShuttle =
    { position: Position
    , momentum: Momentum
    , acceleration: Acceleration
    }

gun: SpaceShuttle -> Position
gun spaceShuttle =
    let
        gunDistance = 7.0
        x = spaceShuttle.position.x + toFloat spaceShuttle.momentum.heading.dx * gunDistance
        y = spaceShuttle.position.y + toFloat spaceShuttle.momentum.heading.dy * gunDistance
    in
        Position x y

--rotate: SpaceShuttle -> Heading -> SpaceShuttle
--rotate spaceShuttle heading =
--    {spaceShuttle|heading=heading}

fire: SpaceShuttle -> Bullet
fire spaceShuttle =
    let
        position = gun spaceShuttle
        heading = spaceShuttle.momentum.heading
        speed = spaceShuttle.momentum.speed + 4
        momentum = Momentum heading speed
        range = 10
    in
        Bullet position momentum range

move: SpaceShuttle -> SpaceShuttle
move spaceShuttle =
    let
        x = spaceShuttle.position.x + toFloat spaceShuttle.momentum.heading.dx * toFloat spaceShuttle.momentum.speed
        y = spaceShuttle.position.y + toFloat spaceShuttle.momentum.heading.dy * toFloat spaceShuttle.momentum.speed
        newPosition = Position x y
    in
        {spaceShuttle|position = newPosition}

thrust: SpaceShuttle -> SpaceShuttle
thrust spaceShuttle =
    {spaceShuttle|acceleration = 1}

accelerate: SpaceShuttle -> SpaceShuttle
accelerate spaceShuttle =
    let
        newSpeed = spaceShuttle.momentum.speed + spaceShuttle.acceleration
        newAcceleration = 0
        oldMomentum = spaceShuttle.momentum
        newMomentum = {oldMomentum|speed = newSpeed}
    in
        { spaceShuttle
            | momentum = newMomentum
            , acceleration = newAcceleration
        }