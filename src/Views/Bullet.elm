module Views.Bullet exposing (bullet)

import Data.Bullet as Bullet exposing (Bullet)
import Svg exposing (..)
import Svg.Attributes exposing (..)

bullet : Bullet -> Svg msg
bullet bullet =
    let
        x = toString bullet.position.x
        y = toString bullet.position.y
        radius = toString (Bullet.getRadius bullet)
    in
        circle [ cx x, cy y, r radius, fill "#555555" ] []
