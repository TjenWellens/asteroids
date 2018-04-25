module Data.Bullet exposing (..)

import Data.Momentum exposing (Momentum)
import Data.Position as Position exposing (Position, move)

type alias Bullet =
    { position: Position
    , momentum: Momentum
    , range: Int
    }

move: Bullet -> Bullet
move bullet =
    let
        position = Position.move bullet.position bullet.momentum.heading bullet.momentum.speed
        momentum = bullet.momentum
        range = bullet.range - 1
    in
        Bullet position momentum range

alive: Bullet -> Bool
alive bullet = bullet.range > 0
