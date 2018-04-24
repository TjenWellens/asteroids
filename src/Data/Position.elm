module Data.Position exposing (..)

type alias Position =
    { x: Int
    , y: Int
    }

type alias Heading =
    { dx: Int
    , dy: Int
    }

move: Position -> Heading -> Position
move position heading =
    Position (position.x + heading.dx) (position.y + heading.dy)