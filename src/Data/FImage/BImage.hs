module Data.FImage.BImage
(
  -- * type
  BImage
)
where

  import qualified Data.FImage.Geometry.Point as Point

  -- | Boolean image (aka region) type definition
  type BImage = Point.Point -> Bool
