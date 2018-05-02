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

type alias Vector = (Float, Float)

sum: Vector -> Vector -> Vector
sum (x1, y1) (x2, y2) =
    ( (x1 + x2), (y1 + y2) )

times: Vector -> Float -> Vector
times (x1, y1) n =
    ( (x1 * n), (y1 * n) )

divide: Vector -> Float -> Vector
divide (x1, y1) n =
    ( (x1 / n), (y1 / n) )

toAngle: Heading -> Float
toAngle heading =
    atan2 (heading.dy) (heading.dx)

toVector: Heading -> Vector
toVector heading = (heading.dx, heading.dy)

fromVector: Vector -> Heading
fromVector (dx, dy) = Heading dx dy

-- (heading, weight)
weighedCombine: (Heading, Float) -> (Heading, Float) -> (Heading, Float)
weighedCombine (heading1, weight1) (heading2, weight2) =
    let
        v1 = toVector heading1
        v2 = toVector heading2
        weight = weight1 + weight2

        vector =
            ( divide
                ( sum
                    (times v1 weight1)
                    (times v2 weight2)
                )
                (weight)
            )
    in
        (fromVector vector, weight)