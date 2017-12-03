module Data.FImage.Geometry.PolarPoint
(
  -- * type
  PolarPoint(..)

  -- * constructing
, mk
)
where

  -- | type definition
  data PolarPoint = PolarPoint { rho :: Float, theta :: Float }
                    deriving (Show, Eq, Ord)

  mk :: Float -> Float -> PolarPoint
  mk r t = PolarPoint { rho = r, theta = t }
