module Views.Bullet exposing (bullets)

import Data.Bullet exposing (Bullet)
import Svg exposing (..)
import Svg.Attributes exposing (..)

bullet : Bullet -> Svg msg
bullet bullet =
    let
        x_1 = toString bullet.x
        y_1 = toString bullet.y
        x_2 = toString (bullet.x - bullet.dx)
        y_2 = toString (bullet.y - bullet.dy)
    in
        line [x1 x_1, y1 y_1, x2 x_2, y2 y_2, stroke "#FFFFFF"] []

bullets : List Bullet -> List (Svg msg)
bullets bullets =
    List.map bullet bullets