module Data.Universe exposing (..)

import Data.Position exposing (Position)

type alias Universe =
    { width: Int
    , height: Int
    }

contains: Universe -> Position -> Bool
contains universe pos =
    True
    && pos.x > 0.0
    && pos.x < toFloat universe.width
    && pos.y > 0.0
    && pos.y < toFloat universe.height

reappear: Universe -> {a|position: Position} -> {a|position: Position}
reappear universe something =
    let
        x = toFloat((floor something.position.x + universe.width) % universe.width)
        y = toFloat((floor something.position.y + universe.height) % universe.height)
    in
        {something|position = Position x y}