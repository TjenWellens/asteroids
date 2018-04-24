module Data.Bullet exposing (..)

import Data.Position exposing (Heading, Position, Velocity, move)

type alias Bullet =
    { position: Position
    , heading: Heading
    , speed: Velocity
    , range: Int
    }

update: Bullet -> Bullet
update bullet =
    let
        position = move bullet.position bullet.heading bullet.speed
        heading = bullet.heading
        speed = bullet.speed
        range = bullet.range - 1
    in
        Bullet position heading speed range

alive: Bullet -> Bool
alive bullet = bullet.range > 0

updateAll: List Bullet -> List Bullet
updateAll bullets =
    bullets
        |> List.map update
        |> List.filter alive