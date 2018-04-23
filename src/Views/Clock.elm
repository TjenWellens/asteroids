module Views.Clock exposing (clock)

import Svg.Attributes exposing (..)
import Time exposing (Time, second)

import Svg exposing (..)

type alias Unit = (Time -> Float)

clock: Time -> List (Svg msg)
clock time =
    [ clockCircle time
    , clockHand time Time.inMinutes
    , clockHand time Time.inHours
    ]

clockCircle: Time -> Svg msg
clockCircle time =
    circle [ cx "50", cy "50", r "45", fill "#0B79CE" ] []

clockHand: Time -> Unit -> Svg msg
clockHand time handType =
    let
      angle =
        turns (handType time)

      handX =
        toString (50 + 40 * cos angle)

      handY =
        toString (50 + 40 * sin angle)
    in
      line [ x1 "50", y1 "50", x2 handX, y2 handY, stroke "#023963" ] []