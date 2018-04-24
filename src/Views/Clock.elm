module Views.Clock exposing (clock)

import Data.Position exposing (Position)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)
import Debug

import Svg exposing (..)

type alias Unit = (Time -> Float)

clock: Time -> List (Svg msg)
clock time =
    let
        center = Position 100 100
    in
        [ clockCircle center time
        , clockHand center time Time.inMinutes
        , clockHand center time Time.inHours
        ]

clockCircle: Position -> Time -> Svg msg
clockCircle center time =
    circle [ cx (toString center.x), cy (toString center.y), r "45", fill "#0B79CE" ] []

clockHand: Position -> Time -> Unit -> Svg msg
clockHand center time handType =
    let
        angle = turns (handType time)

        handX = toString (center.x + 40 * cos angle)

        handY = toString (center.y + 40 * sin angle)
    in
      line [ x1 (toString center.x), y1 (toString center.y), x2 handX, y2 handY, stroke "#023963" ] []