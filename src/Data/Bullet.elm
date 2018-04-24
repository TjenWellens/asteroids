module Data.Bullet exposing (..)

import Data.Position exposing (Heading, Position, move)

type alias Bullet =
    { position: Position
    , heading: Heading
    , range: Int
    }

update: Bullet -> Bullet
update bullet =
    let
        position = move bullet.position bullet.heading
        heading = bullet.heading
        range = bullet.range - 1
    in
        Bullet position heading range

alive: Bullet -> Bool
alive bullet = bullet.range > 0

updateAll: List Bullet -> List Bullet
updateAll bullets =
    bullets
        |> List.map update
        |> List.filter alive