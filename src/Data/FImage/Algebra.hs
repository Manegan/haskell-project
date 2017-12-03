module Data.FImage.Algebra
(
  universe
, empty
, comp
, inter
, union
, xor
, diff
)
where

  import qualified Data.FImage.BImage as BImage
  import qualified Data.FImage.Lift   as Lift

  -- | The all-True boolean image.
  -- 'universe p = True'
  universe :: BImage.BImage

  -- | The all-False boolean image.
  -- 'empty p = False'.
  empty :: BImage.BImage

  -- | Complement a boolean image.
  -- `comp f p = True` iff 'f p = False'.
  comp :: BImage.BImage -> BImage.BImage

  -- | Intersection of two boolean images.
  -- 'inter f g p = True' iff 'f p = True' and 'g p = True'.
  inter :: BImage.BImage -> BImage.BImage -> BImage.BImage

  -- | Union of two boolean images.
  -- 'union f g p = True' iff 'f p = True' or 'g p = True'.
  union :: BImage.BImage -> BImage.BImage -> BImage.BImage

  -- | Xor of two boolean images.
  -- 'xor f g p = True' iff 'f p /= g p'.
  xor :: BImage.BImage -> BImage.BImage -> BImage.BImage

  -- | diff.
  -- 'diff f g p = True' iff 'f p = True' and 'g p = False'.
  diff :: BImage.BImage -> BImage.BImage -> BImage.BImage
