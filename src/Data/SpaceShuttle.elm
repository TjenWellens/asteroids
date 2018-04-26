module Data.SpaceShuttle exposing (..)

import Data.Bullet exposing (Bullet)
import Data.Heading as Heading exposing (Heading)
import Data.Momentum as Momentum exposing (Momentum)
import Data.Rotation exposing (Rotation(..))
import Data.Position exposing (Acceleration, Position)
import Data.Universe exposing (Universe, reappear)

type alias SpaceShuttle =
    { position: Position
    , momentum: Momentum
    , acceleration: Momentum
    , aim: Heading
    }

gun: SpaceShuttle -> Position
gun spaceShuttle =
    let
        gunDistance = 7.0
        x = spaceShuttle.position.x + spaceShuttle.aim.dx * gunDistance
        y = spaceShuttle.position.y + spaceShuttle.aim.dy * gunDistance
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
        shotMomentum = Momentum spaceShuttle.aim (shipAcceleration.speed + speedIncrease)
        bulletMomentum = Momentum.combine shipMomentum shotMomentum
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
        newAcceleration = Momentum spaceShuttle.aim 1
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
        aim = Heading.rotate rotation spaceShuttle.aim
    in
        { spaceShuttle | aim = aim }