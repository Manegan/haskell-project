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
  mk vx vy = Vector { dx = vx, dy = vy }

  -- | Make a vector '(dz, dz)'' from a float 'dz'.
  mk2 :: Float -> Vector
  mk2 z = mk z z

  -- | Make the vector (-dx, dy) from vector (dx, dy).
  revX :: Vector -> Vector
  revX v = Vector { dx = - (dx v), dy = dy v }

  -- | Make the vector (dx, -dy) from vector (dx, dy).
  revY :: Vector -> Vector
  revY v = Vector { dx = dx v, dy = - (dy v) }

  -- | Make the vector (-dx, -dy) from vector (dx, dy).
  revXY :: Vector -> Vector
  revXY v = Vector { dx = - (dx v), dy = - (dy v) }

  -- | Make the vector (1/dx, dy) from vector (dx, dy).
  invX :: Vector -> Vector
  invX v = Vector { dx = 1 / dx v, dy = dy v }

  -- | Make the vector (dx, 1/dy) from vector (dx, dy).
  invY :: Vector -> Vector
  invY v = Vector { dx = dx v, dy = 1 / dy v }

  -- | Make the vector (1/dx, 1/dy) from vector (dx, dy).
  invXY :: Vector -> Vector
  invXY v = Vector { dx = 1 / dx v, dy = 1 / dy v }
