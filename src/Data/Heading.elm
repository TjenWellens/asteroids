module Data.Heading exposing (..)

import Data.Rotation exposing (Rotation(..))
import Data.Vector as Vector exposing (Vector)

type alias Heading = Float

n = turns 0.00
e = turns 0.75
s = turns 0.50
w = turns 0.25

rotationStep = turns 0.05

rotate: Rotation -> Heading -> Heading
rotate rotation =
    case rotation of
        Clockwise -> clockwise
        CounterClockwise -> counterClockwise

clockwise: Heading -> Heading
clockwise heading = heading + rotationStep

counterClockwise: Heading -> Heading
counterClockwise heading = heading - rotationStep

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

        vector =
            Vector.sum
                (Vector.times v1 weight1)
                (Vector.times v2 weight2)

        weight = Vector.length vector
    in
        (fromVector vector, weight)
