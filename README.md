# fimage-squeleton
Functional Image (squeleton)

##

```sh
> git clone git@github.com:your-name/fimage.git
> cd fimage
> cabal sandbox init
> cabal install
> ./.cabal-sandbox/bin/fimage
```

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
changes. (Function ```checker``` is defined in `Data.FImage.BImage.Generator`).

```haskell
checker :: BImage.BImage
checker p = even (x' + y')
  where
    x' = fromIntegral . floor $ Point.x p
    y' = fromIntegral . floor $ Point.y p
```

## Example

```haskell
import qualified Data.FImage.BImage.Generator as BImage.Generator
import qualified Data.FImage.BMP              as BMP
import qualified Data.FImage.View             as View
import qualified Data.FImage.Window           as Window

main :: IO ()
main = do
  let image    = BImage.Generator.wedgeAnnulus 0.27 7 -- generate a functional image
  let view     = View.mk0 3.0 3.0                     -- focus -1.5 <= x <= 1.5 and -1.5 <= y <= 1.5
  let window   = Window.mk 256 256                    -- render a 256x256 bitmap image
  let bmp      = BMP.bmp window view image            -- generate a Codec.BMP.BMP bitmap image
  let filename = "wedgeAnnulus.bmp"                   -- bitmap filename
  BMP.write filename bmp                              -- write wedgeAnnulus.bmp to disk
```

The above program produces the following bmp images (wedgeAnnulus.bmp).

![wedge annulus](/images/wedgeAnnulus.bmp)

## Modules

### Points

The module `Data.FImage.Geometry.Point` defines the `Point` type as follows
```haskell
data Point = Point { x :: Float, y :: Float }
             deriving (Show, Eq, Ord)
```
and provides two construction functions `mk`and `mk2`.

### Polar points

The module `Data.FImage.Geometry.PolarPoint` defines the `PolarPoint` type as
follows
```haskell
data PolarPoint = PolarPoint { rho :: Float, theta :: Float }
                  deriving (Show, Eq, Ord)
```
together with a convenient construction function `mk`.

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

**Define the following functions in `Data.FImage.BImage.Generator`**:
```haskell
import qualified Data.FImage.BImage as BImage

-- | Horizontal half plane at given y coordinate.
hHalfPlane :: Float -> BImage.BImage

-- | Vertical half plane at given x coordinate.
vHalfPlane :: Float -> BImage.BImage

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
`BImage.Simple.vHalfPlane 2`,
`BImage.Generator.hStrip 2`,
`BImage.Generator.uHStrip`,
`BImage.Generator.vStrip 2`,
`BImage.Generator.uVStrip`,
`BImage.Generator.cross 2`,
`BImage.Generator.checker`,
`BImage.Generator.altRings`,
`BImage.Generator.disk 2`,
`BImage.Generator.uDisk`,
`BImage.Generator.square 2`,
`BImage.Generator.uSquare` and
`BImage.Generator.polarChecker 7`
(see testing program `FImage` in `src`):
produce the following bitmaps (display parameters `View.mk0 8 8` and
`Window.mk 256 256`):

![hHalfPlane](/images/hHalfPlane.bmp)
![vHalfPlane](/images/vHalfPlane.bmp)
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
