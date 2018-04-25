module Data.Momentum exposing (..)

import Data.Position as Position exposing (Position)

type alias Momentum =
    { heading: Heading
    , speed: Speed
    }

type alias Heading =
    { dx: Float
    , dy: Float
    }

type alias Speed = Int

n = Heading  0.0 -1.0
e = Heading  1.0  0.0
s = Heading  0.0  1.0
w = Heading -1.0  0.0

none = Momentum n 0
toN = Momentum n 1
toE = Momentum e 1
toS = Momentum s 1
toW = Momentum w 1

type Rotation = Clockwise | CounterClockwise

clockwiseHeading: Heading -> Heading
clockwiseHeading {dx, dy} =
    case (dx, dy) of
        ( 0.0, -1.0) -> e
        ( 1.0,  0.0) -> s
        ( 0.0,  1.0) -> w
        (-1.0,  0.0) -> n
        _ -> Heading  0.0 0.0

counterClockwiseHeading: Heading -> Heading
counterClockwiseHeading {dx, dy} =
    case (dx, dy) of
        ( 0.0, -1.0) -> w
        ( 1.0,  0.0) -> n
        ( 0.0,  1.0) -> e
        (-1.0,  0.0) -> s
        _ -> Heading  0.0 0.0

updateHeading: (Heading -> Heading) -> Momentum -> Momentum
updateHeading changeDirection momentum =
    {momentum|heading = changeDirection momentum.heading}

rotate: Rotation -> Momentum -> Momentum
rotate rotation =
    case rotation of
        Clockwise -> clockwise
        CounterClockwise -> counterClockwise

clockwise: Momentum -> Momentum
clockwise = updateHeading clockwiseHeading

counterClockwise: Momentum -> Momentum
counterClockwise = updateHeading counterClockwiseHeading

move: Momentum -> Position -> Position
move {heading, speed} position =
    let
        x = position.x + (heading.dx * toFloat speed)
        y = position.y + (heading.dy * toFloat speed)
    in
        Position x y

combine: Momentum -> Momentum -> Momentum
combine momentum acceleration =
    let
        heading = acceleration.heading
        speed = momentum.speed + acceleration.speed
    in
        Momentum heading speed