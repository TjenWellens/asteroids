module Views.Bullet exposing (bullets)

import Data.Bullet exposing (Bullet)
import Svg exposing (..)
import Svg.Attributes exposing (..)

bullet : Bullet -> Svg msg
bullet bullet =
    let
        x = toString bullet.position.x
        y = toString bullet.position.y
        radius = "0.7"
    in
        circle [ cx x, cy y, r radius, fill "#aaaaaa" ] []

bullets : List Bullet -> List (Svg msg)
bullets bullets =
    List.map bullet bullets