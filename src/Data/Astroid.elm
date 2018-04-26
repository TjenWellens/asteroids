module Data.Astroid exposing (..)

import Data.Collision as Collision exposing (Collision)
import Data.Momentum as Momentum exposing (Momentum)
import Data.Position as Position exposing (Position)


type alias Astroid =
    { position: Position
    , momentum: Momentum
    , size: Size
    }

type Size = Big | Medium | Small

getRadius: Astroid -> Float
getRadius astroid =
    case astroid.size of
        Big -> 20.0
        Medium -> 10.0
        Small -> 5.0

move: Astroid -> Astroid
move astroid =
    let
        position = Momentum.move astroid.position astroid.momentum
    in
        {astroid | position = position}

smallerSize: Size -> Maybe Size
smallerSize size =
    case size of
        Big -> Just Medium
        Medium -> Just Small
        Small -> Nothing

explode: Astroid -> List Astroid
explode astroid =
    case smallerSize astroid.size of
        Nothing -> []
        Just size ->
            [ Astroid astroid.position Momentum.toN size
            , Astroid astroid.position Momentum.toE size
            , Astroid astroid.position Momentum.toS size
            , Astroid astroid.position Momentum.toW size
            ]

toCollision: Astroid -> Collision
toCollision ({position} as astroid) =
    Collision position (getRadius astroid)

explodeIfCollides: Collision -> Astroid -> List Astroid
explodeIfCollides collision astroid =
    if Collision.collide collision (toCollision astroid) then
        explode astroid
    else
        [ astroid
        ]

collides: Astroid -> Collision -> Bool
collides astroid = Collision.collide (toCollision astroid)