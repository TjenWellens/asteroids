module Views.Astroid exposing (drawAstroid)

import Data.Astroid as Astroid exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)

drawAstroid: Astroid -> Svg msg
drawAstroid astroid =
    let
        x = toString astroid.position.x
        y = toString astroid.position.y
        radius = toString (Astroid.getRadius astroid)
    in
        circle [ cx x, cy y, r radius, fill "#aaaaaa" ] []
