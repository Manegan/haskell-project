module Data.FImage.Geometry.Point
(
  -- * type
  Point(..)

  -- * constructing
, mk
, mk2
)
where

  -- | type definition
  data Point = Point { x :: Float, y :: Float }
               deriving (Show, Eq, Ord)

  -- | Make a point '(x, y)'' from two floats 'x' and 'y'.
  mk :: Float -> Float -> Point
  mk px py = Point { x = px, y = py }

  -- | Make a point '(z, z)'' from a float 'z'.
  mk2 :: Float -> Point
  mk2 z = mk z z
