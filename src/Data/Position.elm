module Data.Position exposing (..)

type alias Position =
    { x: Float
    , y: Float
    }

type alias Heading =
    { dx: Int
    , dy: Int
    }

type alias Speed = Int

type alias Acceleration = Int

move: Position -> Heading -> Speed -> Position
move position heading speed =
    let
        x = position.x + toFloat (heading.dx * speed)
        y = position.y + toFloat (heading.dy * speed)
    in
        Position x y
