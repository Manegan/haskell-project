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
  dist p q = sqrt ((xq - xp) ** 2 + (yq - yp) ** 2)
    where
      xp = Point.x p
      yp = Point.y p
      xq = Point.x q
      yq = Point.y q

  -- | Compute the distance from a given point to the origin.
  distO :: Point.Point -> Float
  distO p = sqrt (xp ** 2 + yp ** 2)
    where
      xp = Point.x p
      yp = Point.y p

  -- | Convert a polar point to a cartesian point.
  fromPolar :: PolarPoint.PolarPoint -> Point.Point
  fromPolar pp =  Point.mk x y
    where
      x = PolarPoint.rho pp * cos (PolarPoint.theta pp)
      y = PolarPoint.rho pp * sin (PolarPoint.theta pp)

  -- | Convert a cartesian point to a polar point.
  toPolar :: Point.Point -> PolarPoint.PolarPoint
  toPolar p = PolarPoint.mk rho theta
    where
      rho   = distO p
      theta = atan2 (Point.y p) (Point.x p)
