module Data.FImage.Geometry
(

  dist
, distO

, fromPolar
, toPolar
)
where

  import qualified Data.FImage.Geometry.Point      as Point
  import qualified Data.FImage.Geometry.PolarPoint as PolarPoint

  -- | Compute the distance betwen two points.
  dist :: Point.Point -> Point.Point -> Float

  -- | Compute the distance from a given point to the origin.
  distO :: Point.Point -> Float

  -- | Convert a polar point to a cartesian point.
  fromPolar :: PolarPoint.PolarPoint -> Point.Point

  -- | Convert a cartesian point to a polar point.
  toPolar :: Point.Point -> PolarPoint.PolarPoint
