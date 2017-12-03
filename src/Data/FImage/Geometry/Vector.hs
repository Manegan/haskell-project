module Data.FImage.Geometry.Vector
(
  -- * type definition
  Vector(..)

  -- * constructing
, mk
, mk2

  -- * transforming
, revX
, revY
, revXY
, invX
, invY
, invXY
)
where

  -- | Vector type definition
  data Vector = Vector { dx :: Float, dy :: Float }
                deriving (Eq, Ord)

  -- | Make a vector (dx, dy) from two floats `dx`and `dy`.
  mk :: Float -> Float -> Vector

  -- | Make a vector '(dz, dz)'' from a float 'dz'.
  mk2 :: Float -> Vector

  -- | Make the vector (-dx, dy) from vector (dx, dy).
  revX :: Vector -> Vector

  -- | Make the vector (dx, -dy) from vector (dx, dy).
  revY :: Vector -> Vector

  -- | Make the vector (-dx, -dy) from vector (dx, dy).
  revXY :: Vector -> Vector

  -- | Make the vector (1/dx, dy) from vector (dx, dy).
  invX :: Vector -> Vector

  -- | Make the vector (dx, 1/dy) from vector (dx, dy).
  invY :: Vector -> Vector

  -- | Make the vector (1/dx, 1/dy) from vector (dx, dy).
  invXY :: Vector -> Vector
