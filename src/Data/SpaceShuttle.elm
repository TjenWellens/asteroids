module Data.SpaceShuttle exposing (..)

import Data.Position exposing (Heading, Position, Velocity)

type alias SpaceShuttle =
    { position: Position
    , heading: Heading
    , speed: Velocity
    }
