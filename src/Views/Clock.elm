module Views.Clock exposing (clock)

import Svg.Attributes exposing (..)
import Time exposing (Time, second)

import Svg exposing (..)

clock: Time -> List (Svg msg)
clock time =
    [ clockCircle time
    , clockHand time
    ]

clockCircle: Time -> Svg msg
clockCircle time =
    circle [ cx "50", cy "50", r "45", fill "#0B79CE" ] []

clockHand: Time -> Svg msg
clockHand time =
    let
      angle =
        turns (Time.inMinutes time)

      handX =
        toString (50 + 40 * cos angle)

      handY =
        toString (50 + 40 * sin angle)
    in
      line [ x1 "50", y1 "50", x2 handX, y2 handY, stroke "#023963" ] []