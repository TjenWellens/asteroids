module Data.Collision exposing (..)

import Data.Position exposing (Position)

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
        (sqrt ((x2 - x1)^2 + (y2 - y1)^2)) < (r1 + r2)
