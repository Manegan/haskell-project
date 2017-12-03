module Data.FImage.Transform
(
  -- * type
  Transform

-- * Transforming boolean images
, translate
, hTranslate
, vTranslate
, scale
, hScale
, vScale
, uScale
, rotate
)
where

  import qualified Data.FImage.BImage          as BImage
  import qualified Data.FImage.Geometry.Point  as Point
  import qualified Data.FImage.Geometry.Vector as Vector

  -- | Point transformation type definition.
  type TransformPoint = Point.Point -> Point.Point

  -- | Boolean image transformation type definition.
  type Transform = BImage.BImage -> BImage.BImage

  -- | Add a vector with a point:
  -- plusPoint (dx, dy) (x, y) = (x + dx, y + dy).
  -- plusPoint :: Vector.Vector -> TransformPoint

  -- | Multiply a vector with a point:
  -- multPoint (dx, dy) (x, y) = (x * dx, y * dy).
  -- multPoint :: Vector.Vector -> TransformPoint

  -- | Translate a point according to a given vector:
  -- translatePoint (dx, dy) (x, y) = (x + dx, y + dy)
  translatePoint :: Vector.Vector -> TransformPoint

  -- | Scale a point according to a given vector:
  -- scalePoint (dx, dy) (x, y) = (x * dx, y * dy).
  scalePoint :: Vector.Vector -> TransformPoint

  -- | Scale uniformaly a point according to a given float:
  -- uScalePoint f (x, y) = (f * x, f * y).
  uScalePoint :: Float -> TransformPoint

  -- | Rotate a point acoording to a given angle t:
  -- rotatePoint t (x, y) = (x cos(t) - y sint(t), x sin(t) + y cos(t)).
  rotatePoint :: Float -> TransformPoint

  -- | Translate a boolean image according to a vector.
  translate :: Vector.Vector -> Transform

  -- | Translate horizontaly a boolean image according to a vector.
  hTranslate :: Float -> Transform

  -- | Translate verticaly a boolean image according to a vector.
  vTranslate :: Float -> Transform

  -- | Scale a boolean image according to a float.
  scale :: Vector.Vector -> Transform

  -- | Scale horizontaly a boolean image according to a float.
  hScale :: Float -> Transform

  -- | Scale verticaly a boolean image according to a float.
  vScale :: Float -> Transform

  -- | Scale uniformaly a boolean image according to a float.
  uScale :: Float -> Transform

  -- | Rotate uniformaly a boolean image according to a float.
  rotate :: Float -> Transform
