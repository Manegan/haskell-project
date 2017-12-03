module Data.FImage.Interval
(
  -- * type
  Interval(..)

  -- * constructing
, mk

  -- * querying
, locate
)
where

  data Interval = Interval { lowerBound :: Float
                           , upperBound :: Float
                           } deriving (Show)

  -- | Make an interval from two floats.
  mk :: Float -> Float -> Interval
  mk lb ub
    | lb > ub   = error "null interval"
    | otherwise = Interval { lowerBound = lb, upperBound = ub }

  -- | Return the length of the interval.
  len :: Interval -> Float
  len i = upperBound i - lowerBound i

  -- | Locate in an interval.
  locate :: Int -> Int -> Interval -> Float
  locate n x i
    | x < 0 || x > n = error "null interval"
    | otherwise      = lowerBound i +  fromIntegral x * len i / fromIntegral n
