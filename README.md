# fimage-squeleton
Functional Image (squeleton)

## Evaluation

Due date: xxx

Format: either as a zip or a GitHub URL

Procedure (in case of GitHub URL):
```sh
> git clone git@github.com:your-name/fimage.git
> cd fimage
> cabal sandbox init
> cabal install
> ./.cabal-sandbox/bin/fimage
```

Bitmap rendering is delegated to the `bmp` haskell library
(https://hackage.haskell.org/package/bmp).
The `FImage.BMP` module is provided as a wrapper so that you can
easily write bitmap images to disk.

## Introduction

*Boolean images* (also called *regions*) are simply functions from infinite 2D
spaces to black and white colors with opacity.

Module `Data.FImage.BImage` defines type the `BImage` as follows:
```haskell
type BImage = Point.Point -> Bool
```
where `Point` is the type of Cartesian coordinates (defined in
`Data.FImage.Geometry.Point`):
```haskell
data Point = Point { x :: Float, y :: Float } deriving (Show, Eq, Ord)
```

As a first example, consider the (infinite) checkered region:

![checker](/images/checker.bmp)

The trick is to take the floor of the pixel coordinates and test whether the sum
is even or odd. Whenever ```x``` or ```y``` passes an integer value, the parity
changes. (Function ```checker``` is defined in `Data.FImage.BImage.Simple`).

```haskell
checker :: BImage.BImage
checker p = even (x' + y')
  where
    x' = fromIntegral . floor $ Point.x p
    y' = fromIntegral . floor $ Point.y p
```

## Modules

First, comment out all calls in the `Main`function of testing program `FImage`
in `src`.

### Points

The module `Data.FImage.Geometry.Point` defines the `Point` type as follows
```haskell
data Point = Point { x :: Float, y :: Float }
             deriving (Show, Eq, Ord)
```
and provides two construction functions `mk`and `mk2`.
The module is complete.

### Polar points

The module `Data.FImage.Geometry.PolarPoint` defines the `PolarPoint` type as
follows
```haskell
data PolarPoint = PolarPoint { rho :: Float, theta :: Float }
                  deriving (Show, Eq, Ord)
```
together with a convenient construction function `mk`.
The module is complete.

### Vectors (**to be completed**)

The module `Data.FImage.Geometry.Vector` defined the `Vector` type as follows.
```haskell
data Vector = Vector { dx :: Float, dy :: Float }
              deriving (Show, Eq, Ord)
```
**Define the following functions in `Data.FImage.Geometry.Vector`**:
```haskell
-- | Make a vector (dx, dy) from two floats `dx`and `dy`.
mk :: Float -> Float -> Vector

-- | Make a vector '(dz, dz)'' from a float 'dz'.
mk2 :: Float -> Vector

-- | Make the vector (-dx, dy) from vector (dx, dy).
revX :: Vector -> Vector

-- | Make the vector (dx, -dy) from vector (dx, dy).
revY :: Vector -> Vector

-- | Make the vector (-dx, -dy) from vector (dx, dy).
revXY :: Vector -> Vector

-- | Make the vector (1/dx, dy) from vector (dx, dy).
invX :: Vector -> Vector

-- | Make the vector (dx, 1/dy) from vector (dx, dy).
invY :: Vector -> Vector

-- | Make the vector (1/dx, 1/dy) from vector (dx, dy).
invXY :: Vector -> Vector
```

### Basic functions (**to be completed**)
Basic geometric functions are defined in the module
`Data.FImage.Geometry`.

**Define the following functions in `Data.FImage.Geometry`**:
```haskell

import qualified Data.FImage.Geometry.Point      as Point
import qualified Data.FImage.Geometry.PolarPoint as PolarPoint

-- | Compute the distance betwen two points.
dist :: Point.Point -> Point.Point -> Float

-- | Compute the distance from a given point to the origin.
distO :: Point.Point -> Float

-- | Convert a polar point to a cartesian point.
fromPolar :: PolarPoint.PolarPoint -> Point.Point

-- | Convert a cartesian point to a polar point.
toPolar :: Point.Point -> PolarPoint.PolarPoint
```

### Generate basic boolean images (to be completed)

The module `Data.FImage.BImage.Simple` contains simple (aka atomic) boolean images.
More advanced boolean images will soon be obtained by combining simple boolean
images with high-order functions in `Data.FImage.Transform` and
`Data.FImage.Transform`.

**Define the following functions in `Data.FImage.BImage.Simple`**:
```haskell
import qualified Data.FImage.BImage as BImage

-- | Horizontal half plane at given y coordinate.
hHalfPlane :: Float -> BImage.BImage

-- | Horizontal half plane at y=0 coordinate.
hHalfPlane0 :: BImage.BImage

-- | Vertical half plane at given x coordinate.
vHalfPlane :: Float -> BImage.BImage

-- | Horizontal half plane at x=0 coordinate.
vHalfPlane0 :: BImage.BImage

-- | Infinitely horizontal strip of given width.
hStrip :: Float -> BImage.BImage

-- | Infinitely horizontal strip of unit width.
uHStrip :: BImage.BImage

-- | Infinitely vertical strip of given width.
vStrip :: Float -> BImage.BImage

-- | Infinitely horizontal strip of unit width.
uVStrip :: BImage.BImage

-- | Checker of unit width.
checker :: BImage.BImage

-- | Concentric of unit width.
altRings :: BImage.BImage

-- | Disk of given radius.
disk :: Float -> BImage.BImage

-- | Disk of unit radius.
uDisk :: BImage.BImage

-- | Square of given length.
square :: Float -> BImage.BImage

-- | Square of unit length.
uSquare :: BImage.BImage

-- | Polar checker. The parameter determines the number of alternations, and
-- hence is twice the number of slices.
-- For a cartesian point (x, y), convert to polar point (t', r') and convert
-- back to a cartesian point (r, t * n + pi) and use checker function.
polarChecker :: Float -> BImage.BImage
```

The functions
`BImage.Simple.hHalfPlane 1`,
`BImage.Simple.hHalfPlane0`,
`BImage.Simple.vHalfPlane 2`,
`BImage.Simple.vHalfPlane0`,
`BImage.Simple.hStrip 2`,
`BImage.Simple.uHStrip`,
`BImage.Simple.vStrip 2`,
`BImage.Simple.uVStrip`,
`BImage.Simple.cross 2`,
`BImage.Simple.checker`,
`BImage.Simple.altRings`,
`BImage.Simple.disk 2`,
`BImage.Simple.uDisk`,
`BImage.Simple.square 2`,
`BImage.Simple.uSquare` and
`BImage.Simple.polarChecker 7`
(see testing program `FImage` in `src`):
produce the following bitmaps (display parameters `View.mk0 8 8` and
`Window.mk 256 256`):

![hHalfPlane](/images/hHalfPlane.bmp)
![hHalfPlane](/images/hHalfPlane0.bmp)
![vHalfPlane](/images/vHalfPlane.bmp)
![vHalfPlane](/images/vHalfPlane0.bmp)
![hStrip](/images/hStrip.bmp)
![uHStrip](/images/uHStrip.bmp)
![vStrip](/images/vStrip.bmp)
![uVStrip](/images/uVStrip.bmp)
![cross](/images/cross.bmp)
![checker](/images/checker.bmp)
![altRings](/images/altRings.bmp)
![disk](/images/disk.bmp)
![uDisk](/images/uDisk.bmp)
![square](/images/square.bmp)
![uSquare](/images/uSquare.bmp)
![polarChecker](/images/polarChecker.bmp)

### Spatial transforms

Spatial transforms are simply defined as space-to-space functions.
The type `Transform` is defined in module `Data.FImage.Transform` as follows:

```haskell
-- | Boolean image transformation type definition.
type Transform = BImage.BImage -> BImage.BImage
```

`Transform` is provided as a friendly type, but most of the work
is devoted to the type `TransformPoint` defined in module `Data.FImage.Transform`
as follows:

```haskell
-- | Point transformation type definition.
type TransformPoint = Point.Point -> Point.Point
```

In brief, `TransformPoint` operates locally whereas `Transform` operates at the
image level.

**Define the following functions in `Data.FImage.Transform`**:

```haskell
-- | Translate a point according to a given vector:
-- translatePoint (dx, dy) (x, y) = (x + dx, y + dy)
translatePoint :: Vector.Vector -> TransformPoint

-- | Scale a point according to a given vector:
-- scalePoint (dx, dy) (x, y) = (x * dx, y * dy).
scalePoint :: Vector.Vector -> TransformPoint

-- | Scale uniformaly a point according to a given float:
-- uScalePoint f (x, y) = (f * x, f * y).
uScalePoint :: Float -> TransformPoint

-- | Rotate a point acoording to a given angle t:
-- rotatePoint t (x, y) = (x cos(t) - y sint(t), x sin(t) + y cos(t)).
rotatePoint :: Float -> TransformPoint

-- | Translate a boolean image according to a vector.
translate :: Vector.Vector -> Transform

-- | Translate horizontaly a boolean image according to a vector.
hTranslate :: Float -> Transform

-- | Translate verticaly a boolean image according to a vector.
vTranslate :: Float -> Transform

-- | Scale a boolean image according to a float.
scale :: Vector.Vector -> Transform

-- | Scale horizontaly a boolean image according to a float.
hScale :: Float -> Transform

-- | Scale verticaly a boolean image according to a float.
vScale :: Float -> Transform

-- | Scale uniformaly a boolean image according to a float.
uScale :: Float -> Transform

-- | Rotate uniformaly a boolean image according to a float.
rotate :: Float -> Transform
```

The type `TransformPoint`and the functions `translatePoint`, `scalePoint`,
`uScalePoint`, and `rotatePoint`
are not publically exposed by the `Data.FImage.Transform` module.

The functions
`translateUSquare 2 3`, `hTranslateUSquare 2`,
`vTranslateUSquare 3`, `scaleUSquare 2 3`,
`hScaleUSquare 2`, `vScaleUSquare 3`,
`uScaleUSquare 3`, `rotateUSquare (pi / 4)` and
`rotateTranslateScaleUSquare (pi/4) 1 1 2 1`
produce the following bitmaps (display parameters `View.mk0 8 8` and
`Window.mk 256 256`):

![translateUSquare](/images/translateUSquare.bmp)
![hTranslateUSquare](/images/hTranslateUSquare.bmp)
![vTranslateUSquare](/images/vTranslateUSquare.bmp)
![scaleUSquare](/images/scaleUSquare.bmp)
![hScaleUSquare](/images/hScaleUSquare.bmp)
![vScaleUSquare](/images/vScaleUSquare.bmp)
![uScaleUSquare](/images/uScaleUSquare.bmp)
![rotateUSquare](/images/rotateUSquare.bmp)
![rotateTranslateScaleUSquare](/images/rotateTranslateScaleUSquare.bmp)

## Boolean image (aka Region) Algebra

Regions of 2D-space (i.e. boolean images) are simply boolean-valued images.
Set operations on regions are useful and easy to define.
We define these set operations in the module
`Data.FImage.Algebra`.

**Define the following functions in `Data.FImage.Algebra`**:

```haskell
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
```
Teacher's advice: you may find useful to use the lift
functions defined in the module `Data.FImage.Lift`
(but this is not mandatory).

```haskell
module Data.FImage.Lift
(
  lift1
, lift2
, lift3
)
where

  lift1 :: (a -> b) -> (p -> a) -> p -> b
  lift1 g f1 p = g (f1 p)

  lift2 :: (a -> b -> c) -> (p -> a) -> (p -> b) -> p -> c
  lift2 g f1 f2 p = g (f1 p) (f2 p)

  lift3 :: (a -> b -> c -> d) -> (p -> a) -> (p -> b) -> (p -> c) -> p -> d
  lift3 g f1 f2 f3 p = g (f1 p) (f2 p) (f3 p)
```

The functions
`BImage.Gallery.diamond 2 3`, `BImage.Gallery.diamond 3 2`,
`BImage.Gallery.oval 2 3` and `BImage.Gallery.oval 3 2`
produce the following bitmaps (display parameters `View.mk0 8 8` and
`Window.mk 256 256`):

![diamond1](/images/diamond1.bmp)
![diamond2](/images/diamond2.bmp)
![oval1](/images/oval1.bmp)
![oval2](/images/oval2.bmp)

## Combining everything

You are now ready to combine transformations
(`Data.FImage.Transform`) and algebra
(`Data.FImage.Algebra`) to obtain surprising boolean images.

![annulus](/images/annulus.bmp)
![checkerDisk](/images/checkerDisk.bmp)
![checkerSquare](/images/checkerSquare.bmp)
![checkerAnnulus](/images/checkerAnnulus.bmp)
![octogon](/images/octogon.bmp)
![xorCircles2](/images/xorCircles2.bmp)
![checkerXORCircles2](/images/checkerXORCircles2.bmp)
![xorCircles4](/images/xorCircles4.bmp)
![xorCircles8](/images/xorCircles8.bmp)
![eclipse](/images/eclipse.bmp)
![lineCircles](/images/lineCircles.bmp)
![timeTunnel](/images/timeTunnel.bmp)

## Animation

Transform a collection of boolean images into a movie.
Watch the video at
http://igm.univ-mlv.fr/~vialette/teaching/2017-2018/Haskell/Project/swirl.mpeg.
