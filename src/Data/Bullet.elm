module Data.Bullet exposing (..)

import Data.Momentum as Momentum exposing (Momentum)
import Data.Position as Position exposing (Position)

type alias Bullet =
    { position: Position
    , momentum: Momentum
    , range: Int
    }

move: Bullet -> Bullet
move bullet =
    let
        position = Momentum.move bullet.position bullet.momentum
        momentum = bullet.momentum
        range = bullet.range - 1
    in
        Bullet position momentum range

alive: Bullet -> Bool
alive bullet = bullet.range > 0
