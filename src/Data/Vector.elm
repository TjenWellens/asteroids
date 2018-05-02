module Data.Vector exposing (..)

type alias Vector = (Float, Float)

sum: Vector -> Vector -> Vector
sum (x1, y1) (x2, y2) =
    ( (x1 + x2), (y1 + y2) )

times: Vector -> Float -> Vector
times (x1, y1) n =
    ( (x1 * n), (y1 * n) )

divide: Vector -> Float -> Vector
divide (x1, y1) n =
    ( (x1 / n), (y1 / n) )

length: Vector -> Float
length vector = distance (0,0) vector

distance: Vector -> Vector -> Float
distance (x1, y1) (x2, y2) = (sqrt ((x2 - x1)^2 + (y2 - y1)^2))