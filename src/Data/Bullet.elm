module Data.Bullet exposing (..)

import Data.Position as Position exposing (Heading, Position, Velocity, move)

type alias Bullet =
    { position: Position
    , heading: Heading
    , speed: Velocity
    , range: Int
    }

move: Bullet -> Maybe Bullet
move bullet =
    let
        position = Position.move bullet.position bullet.heading bullet.speed
        heading = bullet.heading
        speed = bullet.speed
        range = bullet.range - 1
    in
        if bullet.range > 0 then
            Just (Bullet position heading speed range)
        else
            Nothing
