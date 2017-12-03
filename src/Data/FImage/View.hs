module Data.FImage.View
(
  View(..)

  -- * construction
, mk
, mk0

  -- * querying
, xMin
, yMin
, xMax
, yMax
, width
, height
)
where

  import qualified Data.FImage.Geometry.Point as Point

  data View = View { lowerLeft  :: Point.Point
                   , upperRight :: Point.Point
                   } deriving (Show)

  mk :: Point.Point -> Point.Point -> View
  mk ll ur
    | x > x'    = error "View error"
    | y > y'    = error "View error"
    | otherwise = View { lowerLeft = ll, upperRight = ur }
    where
      x  = Point.x ll
      y  = Point.y ll

      x' = Point.x ur
      y' = Point.y ur

  mk0 :: Float -> Float -> View
  mk0 w h = mk ll ur
    where
      ll = Point.mk (-w/2) (-h/2)
      ur = Point.mk (w/2)  (h/2)

  xMin :: View -> Float
  xMin = Point.x . lowerLeft

  yMin :: View -> Float
  yMin = Point.y . lowerLeft

  xMax :: View -> Float
  xMax = Point.x . upperRight

  yMax :: View -> Float
  yMax = Point.x . upperRight

  width :: View -> Float
  width v = xMax v - xMin v + 1

  height :: View -> Float
  height v = yMax v - yMin v + 1
