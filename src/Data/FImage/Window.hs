module Data.FImage.Window
(
  -- * type
  Window(..)

  -- * constructing
, mk
)
where

  data Window = Window { width  :: Int
                       , height :: Int
                       } deriving (Show)

  mk :: Int -> Int -> Window
  mk w h
    | w <= 0    = error "0-width window"
    | h <= 0    = error "0-height window"
    | otherwise = Window { width = w, height = h }
