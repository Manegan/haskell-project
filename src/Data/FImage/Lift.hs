module Data.FImage.Lift
(
  lift1
, lift2
, lift3
)
where

  -- | Lift one level.
  lift1 :: (a -> b) -> (p -> a) -> p -> b
  lift1 g f1 p = g (f1 p)

  -- | Lift two levels.
  lift2 :: (a -> b -> c) -> (p -> a) -> (p -> b) -> p -> c
  lift2 g f1 f2 p = g (f1 p) (f2 p)

  -- | Lift treh levels.
  lift3 :: (a -> b -> c -> d) -> (p -> a) -> (p -> b) -> (p -> c) -> p -> d
  lift3 g f1 f2 f3 p = g (f1 p) (f2 p) (f3 p)
