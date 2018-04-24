module Data.Position exposing (..)

type alias Position =
    { x: Float
    , y: Float
    }

type alias Heading =
    { dx: Int
    , dy: Int
    }

type alias Velocity = Int

move: Position -> Heading -> Velocity -> Position
move position heading speed =
    let
        x = position.x + toFloat (heading.dx * speed)
        y = position.y + toFloat (heading.dy * speed)
    in
        Position x y