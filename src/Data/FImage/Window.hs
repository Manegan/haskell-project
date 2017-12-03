module Data.FImage.Window
(
  -- * type
  Window(..)

  -- * constructing
, mk
)
where

  -- | Window type definition.
  data Window = Window { width  :: Int
                       , height :: Int
                       } deriving (Show)

  -- | Make a window from two integers (width and height).
  mk :: Int -> Int -> Window
  mk w h
    | w <= 0    = error "0-width window"
    | h <= 0    = error "0-height window"
    | otherwise = Window { width = w, height = h }
