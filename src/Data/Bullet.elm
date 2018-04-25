module Data.Bullet exposing (..)

import Data.Heading exposing (Heading, Speed)
import Data.Position as Position exposing (Position, move)

type alias Bullet =
    { position: Position
    , heading: Heading
    , speed: Speed
    , range: Int
    }

move: Bullet -> Bullet
move bullet =
    let
        position = Position.move bullet.position bullet.heading bullet.speed
        heading = bullet.heading
        speed = bullet.speed
        range = bullet.range - 1
    in
        Bullet position heading speed range

alive: Bullet -> Bool
alive bullet = bullet.range > 0
