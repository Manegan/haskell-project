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
  universe = const True

  -- | The all-False boolean image.
  -- 'empty p = False'.
  empty :: BImage.BImage
  empty = const False

  -- | Complement a boolean image.
  -- `comp f p = True` iff 'f p = False'.
  comp :: BImage.BImage -> BImage.BImage
  comp = Lift.lift1 not

  -- | Intersection of two boolean images.
  -- 'inter f g p = True' iff 'f p = True' and 'g p = True'.
  inter :: BImage.BImage -> BImage.BImage -> BImage.BImage
  inter = Lift.lift2 (&&)

  -- | Union of two boolean images.
  -- 'union f g p = True' iff 'f p = True' or 'g p = True'.
  union :: BImage.BImage -> BImage.BImage -> BImage.BImage
  union = Lift.lift2 (||)

  -- | Xor of two boolean images.
  -- 'xor f g p = True' iff 'f p /= g p'.
  xor :: BImage.BImage -> BImage.BImage -> BImage.BImage
  xor = Lift.lift2 (/=)

  -- | diff.
  -- 'diff f g p = True' iff 'f p = True' and 'g p = False'.
  diff :: BImage.BImage -> BImage.BImage -> BImage.BImage
  f `diff` g = f `inter` comp g
