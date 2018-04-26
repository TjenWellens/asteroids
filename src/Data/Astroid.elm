module Data.Astroid exposing (..)

import Data.Momentum as Momentum exposing (Momentum)
import Data.Position as Position exposing (Position)


type alias Astroid =
    { position: Position
    , momentum: Momentum
    , size: Size
    }

type Size = Big | Medium | Small

getRadius: Astroid -> Float
getRadius astroid =
    case astroid.size of
        Big -> 20.0
        Medium -> 10.0
        Small -> 5.0

move: Astroid -> Astroid
move astroid =
    let
        position = Momentum.move astroid.position astroid.momentum
    in
        {astroid | position = position}