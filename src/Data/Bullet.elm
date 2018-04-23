module Data.Bullet exposing (..)

type alias Bullet =
    { x: Int
    , y: Int
    , dx: Int
    , dy: Int
    , range: Int
    }

update: Bullet -> Bullet
update bullet =
    let
        dx = bullet.dx
        dy = bullet.dy
        x = bullet.x + dx
        y = bullet.y + dy
        range = bullet.range - 1
    in
        Bullet x y dx dy range

alive: Bullet -> Bool
alive bullet = bullet.range > 0

updateAll: List Bullet -> List Bullet
updateAll bullets =
    bullets
        |> List.map update
        |> List.filter alive