module Data.SpaceShuttle exposing (..)

import Data.Bullet exposing (Bullet)
import Data.Momentum as Momentum exposing (Heading, Momentum, Rotation, Speed)
import Data.Position exposing (Acceleration, Position)
import Data.Universe exposing (Universe, reappear)

type alias SpaceShuttle =
    { position: Position
    , momentum: Momentum
    , acceleration: Momentum
    }

gun: SpaceShuttle -> Position
gun spaceShuttle =
    let
        gunDistance = 7.0
        x = spaceShuttle.position.x + spaceShuttle.acceleration.heading.dx * gunDistance
        y = spaceShuttle.position.y + spaceShuttle.acceleration.heading.dy * gunDistance
    in
        Position x y

fire: SpaceShuttle -> Bullet
fire spaceShuttle =
    let
        speedIncrease = 4
        range = 10
        startPosition = gun spaceShuttle

        shipMomentum = spaceShuttle.momentum
        shipAcceleration = spaceShuttle.acceleration
        bulletAcceleration = {shipAcceleration|speed = shipAcceleration.speed + speedIncrease}
        bulletMomentum = Momentum.combine shipMomentum bulletAcceleration
    in
        Bullet startPosition bulletMomentum range

move: SpaceShuttle -> SpaceShuttle
move spaceShuttle =
    let
        newPosition = Momentum.move spaceShuttle.momentum spaceShuttle.position
    in
        {spaceShuttle|position = newPosition}

thrust: SpaceShuttle -> SpaceShuttle
thrust spaceShuttle =
    let
        oldAcceleration = spaceShuttle.acceleration
        newAcceleration = {oldAcceleration | speed = 1}
    in
        {spaceShuttle|acceleration = newAcceleration}

accelerate: SpaceShuttle -> SpaceShuttle
accelerate spaceShuttle =
    let
        oldAcceleration = spaceShuttle.acceleration
        newMomentum = Momentum.combine spaceShuttle.momentum spaceShuttle.acceleration
        newAcceleration = {oldAcceleration|speed = 0}
    in
        { spaceShuttle
            | momentum = newMomentum
            , acceleration = newAcceleration
        }

rotate: Rotation -> SpaceShuttle -> SpaceShuttle
rotate rotation spaceShuttle =
    let
        newAcceleration = Momentum.rotate rotation spaceShuttle.acceleration
    in
        { spaceShuttle | acceleration = newAcceleration }