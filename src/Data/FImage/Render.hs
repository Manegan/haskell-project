{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Data.FImage.Render
(
  Render(..)
)
where

  import qualified Data.Word as Word

  -- | Render type class.
  class Render a where
    render :: a -> [Word.Word8]

  -- | Render a boolean as four word8.
  instance Render Bool where
    render False = [255, 255, 255, 255]
    render True  = [  0,   0,   0, 255]
