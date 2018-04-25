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
        x = spaceShuttle.position.x + toFloat spaceShuttle.momentum.heading.dx * gunDistance
        y = spaceShuttle.position.y + toFloat spaceShuttle.momentum.heading.dy * gunDistance
    in
        Position x y

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
        newPosition = Momentum.move spaceShuttle.momentum spaceShuttle.position
    in
        {spaceShuttle|position = newPosition}

thrust: SpaceShuttle -> SpaceShuttle
thrust spaceShuttle =
    let
        momentum = spaceShuttle.momentum
        acceleration = {momentum | speed = 1}
    in
        {spaceShuttle|acceleration = acceleration}

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
        newHeading = (Momentum.rotate rotation spaceShuttle.momentum).heading
        oldMomentum     = spaceShuttle.momentum
        oldAcceleration = spaceShuttle.acceleration
        newMomentum     = {oldMomentum     | heading = newHeading}
        newAcceleration = {oldAcceleration | heading = newHeading}
    in
        { spaceShuttle
            | momentum = newMomentum
            , acceleration = newAcceleration
        }