module Data.Heading exposing (..)

import Data.Rotation exposing (Rotation(..))

type alias Heading =
    { dx: Float
    , dy: Float
    }

n = Heading  0.0 -1.0
e = Heading  1.0  0.0
s = Heading  0.0  1.0
w = Heading -1.0  0.0

rotate: Rotation -> Heading -> Heading
rotate rotation =
    case rotation of
        Clockwise -> clockwise
        CounterClockwise -> counterClockwise

clockwise: Heading -> Heading
clockwise {dx, dy} =
    case (dx, dy) of
        ( 0.0, -1.0) -> e
        ( 1.0,  0.0) -> s
        ( 0.0,  1.0) -> w
        (-1.0,  0.0) -> n
        _ -> Heading  0.0 0.0

counterClockwise: Heading -> Heading
counterClockwise {dx, dy} =
    case (dx, dy) of
        ( 0.0, -1.0) -> w
        ( 1.0,  0.0) -> n
        ( 0.0,  1.0) -> e
        (-1.0,  0.0) -> s
        _ -> Heading  0.0 0.0

sum: Heading -> Heading -> Heading
sum a b =
    Heading (a.dx + b.dx) (a.dy + b.dy)

times: Heading -> Float -> Heading
times a n =
    Heading (a.dx * n) (a.dy * n)

divide: Heading -> Float -> Heading
divide a n =
    Heading (a.dx / n) (a.dy / n)
