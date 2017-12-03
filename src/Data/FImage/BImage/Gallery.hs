module Data.FImage.BImage.Gallery
(
  diamond
, oval
, annulus
, checkerDisk
, checkerSquare
, checkerAnnulus
, octogon
, xorCircles2
, checkerXORCircles2
, xorCircles4
, xorCircles8
, eclipse
, lineCircles
, timeTunnel
)
where

  import qualified Data.Foldable as F

  import qualified Data.FImage.BImage          as BImage
  import qualified Data.FImage.BImage.Simple   as BImage.Simple
  import qualified Data.FImage.Algebra         as Algebra
  import qualified Data.FImage.Transform       as Transform
  import qualified Data.FImage.Geometry.Vector as Vector

  -- translate :: Float -> Float -> BImage.BImage -> BImage.BImage
  -- translate dx dy = Transform.translate (Vector.mk dx dy)

  scale :: Float -> Float -> BImage.BImage -> BImage.BImage
  scale dx dy = Transform.scale (Vector.mk dx dy)

  rotate :: Float -> BImage.BImage -> BImage.BImage
  rotate = Transform.rotate

  diamond :: Float -> Float -> BImage.BImage
  diamond w h = scale (w / sqrt 2) (h / sqrt 2) $ rotate (pi / 4) BImage.Simple.uSquare

  oval :: Float -> Float -> BImage.BImage
  oval w h = scale (w / sqrt 2) (h / sqrt 2) BImage.Simple.uDisk

  annulus :: Float -> Float -> BImage.BImage
  annulus f f' = d `Algebra.diff` d'
    where
      d  = Transform.uScale f  BImage.Simple.uDisk
      d' = Transform.uScale f' BImage.Simple.uDisk

  checkerDisk :: Float -> BImage.BImage
  checkerDisk f = BImage.Simple.disk f `Algebra.inter` BImage.Simple.checker

  checkerSquare :: Float -> BImage.BImage
  checkerSquare f = BImage.Simple.square f `Algebra.inter` BImage.Simple.checker

  checkerAnnulus :: Float -> Float -> BImage.BImage
  checkerAnnulus f f' = annulus f f' `Algebra.inter` BImage.Simple.checker

  octogon :: Float -> BImage.BImage
  octogon f = strip0 `Algebra.inter` strip1 `Algebra.inter` strip2 `Algebra.inter` strip3
    where
      strip0 = BImage.Simple.hStrip f
      strip1 = Transform.rotate (pi / 4) strip0
      strip2 = Transform.rotate (pi / 4) strip1
      strip3 = Transform.rotate (pi / 4) strip2

  xorCircles2 :: Float -> Float -> BImage.BImage
  xorCircles2 f f' = lDisk `Algebra.xor` rDisk
    where
      disk  = BImage.Simple.disk f
      lDisk = Transform.hTranslate (-f') disk
      rDisk = Transform.hTranslate f'    disk

  checkerXORCircles2 :: Float -> Float -> BImage.BImage
  checkerXORCircles2 f f' = BImage.Simple.checker `Algebra.xor` xorCircles2 f f'

  xorCircles4 :: Float -> Float -> BImage.BImage
  xorCircles4 f f' = i `Algebra.xor` Transform.rotate (pi / 2) i
    where
      i  = xorCircles2 f f'

  xorCircles8 :: Float -> Float -> BImage.BImage
  xorCircles8 f f' = i `Algebra.xor` Transform.rotate (pi / 4) i
    where
      i  = xorCircles4 f f'

  eclipse :: Float -> Float -> Float -> BImage.BImage
  eclipse f f' f'' = d `Algebra.diff` d'
    where
      d  = BImage.Simple.disk f
      d' = Transform.hTranslate f' $ Transform.uScale f'' d

  lineCircles :: Float -> BImage.BImage
  lineCircles f = F.foldr1 Algebra.xor ds
    where
      ds = [mk dx | dx <- [-4,-3..4]]
      mk dx = Transform.uScale (abs dx) $ Transform.hTranslate dx (BImage.Simple.disk f)

  timeTunnel :: BImage.BImage
  timeTunnel = F.foldr1 Algebra.xor hps
    where
      hps = [Transform.rotate (i * pi / 16) $ BImage.Simple.vHalfPlane 0 | i <- [1..16]]
