{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Data.FImage.BImage.Simple
(
  -- * Basic boolean images
  hHalfPlane
, hHalfPlane0
, vHalfPlane
, vHalfPlane0
, hStrip
, uHStrip
, vStrip
, uVStrip
, cross
, checker
, altRings
, disk
, uDisk
, square
, uSquare
, polarChecker
)
where

  import qualified Data.FImage.BImage              as BImage
  import qualified Data.FImage.Geometry            as Geometry
  import qualified Data.FImage.Geometry.Point      as Point
  import qualified Data.FImage.Geometry.PolarPoint as PolarPoint

  -- | Horizontal half plane at given y coordinate.
  hHalfPlane :: Float -> BImage.BImage
  hHalfPlane y p = Point.y p <= y

  -- | Horizontal half plane at y=0 coordinate.
  hHalfPlane0 :: BImage.BImage
  hHalfPlane0 = hHalfPlane 0

  -- | Vertical half plane at given x coordinate.
  vHalfPlane :: Float -> BImage.BImage
  vHalfPlane x p = Point.x p <= x

  -- | Horizontal half plane at x=0 coordinate.
  vHalfPlane0 :: BImage.BImage
  vHalfPlane0 = vHalfPlane 0

  -- | Infinitely horizontal strip of given width.
  hStrip :: Float -> BImage.BImage
  hStrip w p = abs (Point.y p) <= w / 2

  -- | Infinitely horizontal strip of unit width.
  uHStrip :: BImage.BImage
  uHStrip = hStrip 1

  -- | Infinitely vertical strip of given width.
  vStrip :: Float -> BImage.BImage
  vStrip w p = abs (Point.x p) <= w / 2

  -- | Infinitely horizontal strip of unit width.
  uVStrip :: BImage.BImage
  uVStrip = vStrip 1

  -- | Infinitely horizontal and vertical strips of given width.
  cross :: Float -> BImage.BImage
  cross w p = abs (Point.x p) <= w / 2 || abs (Point.y p) <= w / 2

  -- | Checker of unit width.
  checker :: BImage.BImage
  checker p = even (x' + y')
    where
      x' = fromIntegral . floor $ Point.x p
      y' = fromIntegral . floor $ Point.y p

  -- | Concentric of unit width.
  altRings :: BImage.BImage
  altRings p = even . fromIntegral . floor $ Geometry.distO p

  -- | Disk of given radius.
  disk :: Float -> BImage.BImage
  disk r p = Geometry.distO p <= r

  -- | Disk of unit radius.
  uDisk :: BImage.BImage
  uDisk = disk 1

  -- | Square of given length.
  square :: Float -> BImage.BImage
  square l p = max x y <= l / 2
    where
      x = abs (Point.x p)
      y = abs (Point.y p)

  -- | Square of unit length.
  uSquare :: BImage.BImage
  uSquare = square 1.0

  -- | Polar checker. The parameter determines the number of alternations, and
  -- hence is twice the number of slices.
  -- For a cartesian point (x, y), convert to polar point (t', r') and convert
  -- back to a cartesian point (r, t * n + pi) and use checker function.
  polarChecker :: Float -> BImage.BImage
  polarChecker f = checker . aux . Geometry.toPolar
    where
      aux p = Point.mk x y
        where
          x = PolarPoint.rho p
          y = f / pi * PolarPoint.theta p
