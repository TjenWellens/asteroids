module Data.Position exposing (..)

type alias Position =
    { x: Int
    , y: Int
    }

type alias Heading =
    { dx: Int
    , dy: Int
    }

type alias Velocity = Int

move: Position -> Heading -> Velocity -> Position
move position heading speed =
    Position (position.x + heading.dx * speed) (position.y + heading.dy * speed)