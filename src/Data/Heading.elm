module Data.Heading exposing (..)

type alias Heading =
    { dx: Int
    , dy: Int
    }

n = Heading  0 -1
e = Heading  1  0
s = Heading  0  1
w = Heading -1  0