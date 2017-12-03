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

  -- | View type definition.
  data View = View { lowerLeft  :: Point.Point
                   , upperRight :: Point.Point
                   } deriving (Show)

  -- | Make an axis-parallel view from the lower-left point and the
  -- upper-right point.d
  mk :: Point.Point -> Point.Point -> View

  -- | Make an 0center axis-parallel view from two floats (width and height).
  mk0 :: Float -> Float -> View

  -- | min x value.
  xMin :: View -> Float
  xMin = Point.x . lowerLeft

  -- | min y value.
  yMin :: View -> Float
  yMin = Point.y . lowerLeft

  -- | yMax x value.
  xMax :: View -> Float
  xMax = Point.x . upperRight

  -- | max y value.
  yMax :: View -> Float
  yMax = Point.x . upperRight

  width :: View -> Float
  width v = xMax v - xMin v + 1

  height :: View -> Float
  height v = yMax v - yMin v + 1
