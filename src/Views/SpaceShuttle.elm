module Views.SpaceShuttle exposing (spaceShuttle)

import Svg exposing (..)
import Svg.Attributes exposing (..)

spaceShuttle model =
    [ polygon [ points "50,44 47,53 53,53", stroke "#FFFFFF" ] [] ]