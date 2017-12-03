{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Data.FImage.Render
(
  Render(..)
)
where

  import qualified Data.Word as Word

  class Render a where
    render :: a -> [Word.Word8]

  instance Render Bool where
    render False = [255, 255, 255, 255]
    render True  = [  0,   0,   0, 255]
