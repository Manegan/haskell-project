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
  -- import qualified Data.FImage.Geometry        as Geometry
  import qualified Data.FImage.Geometry.Point  as Point
  import qualified Data.FImage.Geometry.Vector as Vector

  -- | Point transformation type definition.
  type TransformPoint = Point.Point -> Point.Point

  -- | Boolean image transformation type definition.
  type Transform = BImage.BImage -> BImage.BImage

  -- | Add a vector with a point:
  -- plusPoint (dx, dy) (x, y) = (x + dx, y + dy).
  plusPoint :: Vector.Vector -> TransformPoint
  plusPoint v p = Point.mk x y
    where
      x = Point.x p + Vector.dx v
      y = Point.y p + Vector.dy v

  -- | Multiply a vector with a point:
  -- multPoint (dx, dy) (x, y) = (x * dx, y * dy).
  multPoint :: Vector.Vector -> TransformPoint
  multPoint v p = Point.mk x y
    where
      x = Point.x p * Vector.dx v
      y = Point.y p * Vector.dy v

  -- | Translate a point according to a given vector:
  -- translatePoint (dx, dy) (x, y) = (x + dx, y + dy)
  translatePoint :: Vector.Vector -> TransformPoint
  translatePoint = plusPoint

  -- | Scale a point according to a given vector:
  -- scalePoint (dx, dy) (x, y) = (x * dx, y * dy).
  scalePoint :: Vector.Vector -> TransformPoint
  scalePoint = multPoint

  -- | Scale uniformaly a point according to a given float:
  -- uScalePoint f (x, y) = (f * x, f * y).
  uScalePoint :: Float -> TransformPoint
  uScalePoint = scalePoint . Vector.mk2

  -- | Rotate a point acoording to a given angle t:
  -- rotatePoint t (x, y) = (x cos(t) - y sint(t), x sin(t) + y cos(t)).
  rotatePoint :: Float -> TransformPoint
  rotatePoint t p = Point.mk x y
    where
      x = Point.x p * cos t - Point.y p * sin t
      y = Point.x p * sin t + Point.y p * cos t

  -- | Translate a boolean image according to a vector.
  translate :: Vector.Vector -> Transform
  translate v i = i . translatePoint (Vector.revXY v)

  -- | Translate horizontaly a boolean image according to a vector.
  hTranslate :: Float -> Transform
  hTranslate f = translate v
    where
      v = Vector.mk f 0

  -- | Translate verticaly a boolean image according to a vector.
  vTranslate :: Float -> Transform
  vTranslate f = translate v
    where
      v = Vector.mk 0 f

  -- | Scale a boolean image according to a float.
  scale :: Vector.Vector -> Transform
  scale v i = i . scalePoint (Vector.invXY v)

  -- | Scale horizontaly a boolean image according to a float.
  hScale :: Float -> Transform
  hScale f = scale v
    where
      v = Vector.mk f 1

  -- | Scale verticaly a boolean image according to a float.
  vScale :: Float -> Transform
  vScale f = scale v
    where
      v = Vector.mk 1 f

  -- | Scale uniformaly a boolean image according to a float.
  uScale :: Float -> Transform
  uScale s i = i . uScalePoint (1 / s)

  -- | Rotate uniformaly a boolean image according to a float.
  rotate :: Float -> Transform
  rotate t i = i . rotatePoint (-t)
