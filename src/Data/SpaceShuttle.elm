module Data.SpaceShuttle exposing (..)

import Data.Bullet exposing (Bullet)
import Data.Collision as Collision exposing (Collision)
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
    , livelyness: Livelyness
    }

type Livelyness = Alive | Dead

gun: SpaceShuttle -> Position
gun spaceShuttle =
    let
        gunDistance = 7
        towards = Momentum spaceShuttle.aim gunDistance
        bar = Momentum.move spaceShuttle.position towards
    in
        bar
fire: SpaceShuttle -> Bullet
fire spaceShuttle =
    let
        speedIncrease = 4.0
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
        newPosition = Momentum.move spaceShuttle.position spaceShuttle.momentum
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
        newAcceleration = {oldAcceleration|speed = 0.0}
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

getRadius: SpaceShuttle -> Float
getRadius _ = 6

toCollision: SpaceShuttle -> Collision
toCollision ({position} as spaceShuttle) =
    Collision position (getRadius spaceShuttle)

collides: SpaceShuttle -> Collision -> Bool
collides spaceShuttle = Collision.collide (toCollision spaceShuttle)

explode: SpaceShuttle -> SpaceShuttle
explode spaceShuttle = {spaceShuttle|livelyness=Dead}

explodeIf: Bool -> SpaceShuttle -> SpaceShuttle
explodeIf shouldExplode spaceShuttle =
    if shouldExplode then
        explode spaceShuttle
    else
        spaceShuttle
