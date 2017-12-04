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
  dist p1 p2 = sqrt $ ((Point.x p2 - Point.x p1)**2) + ((Point.y p2 - Point.y p1)**2)

  -- | Compute the distance from a given point to the origin.
  distO :: Point.Point -> Float
  distO p = sqrt $ Point.Point.x p ** 2 + Point.Point.y p ** 2

  -- | Convert a polar point to a cartesian point.
  fromPolar :: PolarPoint.PolarPoint -> Point.Point
  fromPolar p = Point.mk $ (PolarPoint.PolarPoint.r p * cos $ PolarPoint.PolarPoint.t p) (PolarPoint.PolarPoint.r p * sin $ PolarPoint.PolarPoint.t p)

  -- | Convert a cartesian point to a polar point.
  toPolar :: Point.Point -> PolarPoint.PolarPoint
  toPolar p = PolarPoint.PolarPoint.mk (dist p) (t' p)
    where t' p = atan $ Point.Point.y p / Point.Point.x p
