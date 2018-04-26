module Data.Bullet exposing (..)

import Data.Collision as Collision exposing (Collision)
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

getRadius: Bullet -> Float
getRadius _ = 0.7

explode: Bullet -> List Bullet
explode bullet = []

toCollision: Bullet -> Collision
toCollision ({position} as bullet) =
    Collision position (getRadius bullet)

explodeIfCollides: Collision -> Bullet -> List Bullet
explodeIfCollides collision bullet =
    if Collision.collide collision (toCollision bullet) then
        explode bullet
    else
        [ bullet
        ]

collides: Bullet -> Collision -> Bool
collides bullet = Collision.collide (toCollision bullet)
