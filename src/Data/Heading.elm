module Data.Heading exposing (..)

import Data.Rotation exposing (Rotation(..))

type alias Heading = Float

n = turns 0.00
e = turns 0.75
s = turns 0.50
w = turns 0.25

rotationStep = turns 0.1

rotate: Rotation -> Heading -> Heading
rotate rotation =
    case rotation of
        Clockwise -> clockwise
        CounterClockwise -> counterClockwise

clockwise: Heading -> Heading
clockwise heading = heading + rotationStep

counterClockwise: Heading -> Heading
counterClockwise heading = heading - rotationStep

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
toAngle heading = heading

toVector: Heading -> Vector
toVector heading =
    let
        angle = toAngle heading
    in
        (cos angle, sin angle)

fromVector: Vector -> Heading
fromVector (dx, dy)= radians (atan2 dy dx)

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
