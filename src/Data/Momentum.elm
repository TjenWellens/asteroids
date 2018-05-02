module Data.Momentum exposing (..)

import Data.Heading as Heading exposing (Heading)
import Data.Position as Position exposing (Position)
import Data.Rotation exposing (Rotation(..))

type alias Momentum =
    { heading: Heading
    , speed: Speed
    }

type alias Speed = Int

none = Momentum Heading.n 0
toN = Momentum Heading.n 1
toE = Momentum Heading.e 1
toS = Momentum Heading.s 1
toW = Momentum Heading.w 1

updateHeading: (Heading -> Heading) -> Momentum -> Momentum
updateHeading changeDirection momentum =
    {momentum|heading = changeDirection momentum.heading}

rotate: Rotation -> Momentum -> Momentum
rotate rotation =
    case rotation of
        Clockwise -> clockwise
        CounterClockwise -> counterClockwise

clockwise: Momentum -> Momentum
clockwise = updateHeading Heading.clockwise

counterClockwise: Momentum -> Momentum
counterClockwise = updateHeading Heading.counterClockwise

move: Position -> Momentum -> Position
move position {heading, speed} =
    let
        (dx, dy) = Heading.toVector heading
        x = position.x + (dx * toFloat speed)
        y = position.y + (dy * toFloat speed)
    in
        Position x y

combine: Momentum -> Momentum -> Momentum
combine momentum acceleration =
    case (momentum.speed, acceleration.speed) of
        (0,0) -> none
        (1,0) -> momentum
        (0,1) -> acceleration
        _ -> weighedCombine momentum acceleration

weighedCombine: Momentum -> Momentum -> Momentum
weighedCombine momentum acceleration =
    let
        speed = momentum.speed + acceleration.speed
        heading =
            ( Heading.divide
                ( Heading.sum
                    (Heading.times momentum.heading (toFloat momentum.speed))
                    (Heading.times acceleration.heading (toFloat acceleration.speed))
                )
                (toFloat speed)
            )
    in
        Momentum heading speed