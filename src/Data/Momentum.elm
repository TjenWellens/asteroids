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

clockwise: Heading -> Heading
clockwise {dx, dy} =
    case (dx, dy) of
        ( 0, -1) -> e
        ( 1,  0) -> s
        ( 0,  1) -> w
        (-1,  0) -> n
        _ -> Heading  0 0

counterClockwise: Heading -> Heading
counterClockwise {dx, dy} =
    case (dx, dy) of
        ( 0, -1) -> w
        ( 1,  0) -> n
        ( 0,  1) -> e
        (-1,  0) -> s
        _ -> Heading  0 0