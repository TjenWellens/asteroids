module Data.Collision exposing (..)

import Data.Position exposing (Position)
import Data.Vector as Vector

type alias Collision =
    { position: Position
    , radius: Float
    }

collide: Collision -> Collision -> Bool
collide c1 c2 =
    let
        x1 = c1.position.x
        y1 = c1.position.y
        r1 = c1.radius
        x2 = c2.position.x
        y2 = c2.position.y
        r2 = c2.radius
    in
        (Vector.distance (x1, y1) (x2, y2)) < (r1 + r2)

explodeIfCollides: (obstacle -> Collision) -> (b -> Collision -> Bool) -> (Bool -> b -> result) -> List obstacle -> b -> result
explodeIfCollides obstacleToCollision collide explode obstacles b =
    let
        shouldExplode = obstacles
            |> List.map obstacleToCollision
            |> List.any (collide b)
    in
        explode shouldExplode b
