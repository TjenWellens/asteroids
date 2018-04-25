module Data.Momentum exposing (..)

type alias Momentum =
    { heading: Heading
    , speed: Speed
    }

type alias Heading =
    { dx: Int
    , dy: Int
    }

type alias Speed = Int

n = Heading  0 -1
e = Heading  1  0
s = Heading  0  1
w = Heading -1  0

type Rotation = Clockwise | CounterClockwise

clockwiseHeading: Heading -> Heading
clockwiseHeading {dx, dy} =
    case (dx, dy) of
        ( 0, -1) -> e
        ( 1,  0) -> s
        ( 0,  1) -> w
        (-1,  0) -> n
        _ -> Heading  0 0

counterClockwiseHeading: Heading -> Heading
counterClockwiseHeading {dx, dy} =
    case (dx, dy) of
        ( 0, -1) -> w
        ( 1,  0) -> n
        ( 0,  1) -> e
        (-1,  0) -> s
        _ -> Heading  0 0

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
