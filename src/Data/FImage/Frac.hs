module Data.FImage.Frac
(
  Frac(..)

, mk
)
where

  newtype Frac = Frac { getVal :: Float } deriving (Eq, Ord)

  instance Show Frac where
    show = show . getVal

  mk :: Float -> Frac
  mk x
    | x < 0 || x > 1 = error "bad frac float"
    | otherwise      = Frac { getVal = x }
