# fimage-squeleton
Functional Image (squeleton)

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
  let window   = Window.mk 512 512                    -- render a 512x512 bitmap image
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

### Basic boolean images (to be completed)

```haskell
import qualified Data.FImage.BImage as BImage

-- | Infinitely horizontal strip of  given width.
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
Obtained bitmap images are (with display parameters `View.mk0 8 8` and
`Window.mk 256 256`):

![hStrip](/images/hStrip.bmp)
![uHStrip](/images/uHStrip.bmp)
![vStrip](/images/vStrip.bmp)
![uVStrip](/images/uVStrip.bmp)
![checker](/images/checker.bmp)
![altRings](/images/altRings.bmp)
![disk](/images/disk.bmp)
![uDisk](/images/uDisk.bmp)
![square](/images/square.bmp)
![uSquare](/images/uSquare.bmp)
![polarChecker](/images/polarChecker.bmp)

### Spatial transforms

Spatial transforms are simply defined as space-to-space functions
(transforms map points to points)
The type `Transform` is defined in module `Data.FImage.Transform` as follows:

```haskell
import qualified Data.FImage.Geometry.Point  as Point

-- | Spatial transformation type definition
type Transform = Point.Point -> Point.Point
```
**Define the following functions in `Data.FImage.Transform`**:
```haskell
type Transform = Point.Point -> Point.Point

-- |
-- | Translate according to a given vector (dx, dy).
translate :: Vector.Vector -> Transform

-- | Scale according to a given vector (dx, dy).
scale :: Vector.Vector -> Transform

-- | Scale according to a given vector (dx, dy) with dx = dy.
uScale :: Float -> Transform

-- | Rotate acoording to a given angle t.
-- For a given point (x, y) the roated point is defined by
-- (x cos(t) - y sint(t), x sin(t) + y cos(t)).
rotate :: Float -> Transform
```
The following functions:
```haskell
translateUSquare :: (String, BImage.BImage)
translateUSquare = ("translateUSquare.bmp",  BImage.Generator.uSquare . Transform.translate v)
  where
    v = Vector.mk 2 3

hTranslateUSquare :: (String, BImage.BImage)
hTranslateUSquare =  ("hTranslateUSquare.bmp",  BImage.Generator.uSquare . Transform.hTranslate 3)

vTranslateUSquare :: (String, BImage.BImage)
vTranslateUSquare =  ("vTranslateUSquare.bmp",  BImage.Generator.uSquare . Transform.vTranslate 3)

scaleUSquare :: (String, BImage.BImage)
scaleUSquare = ("scaleUSquare.bmp",  BImage.Generator.uSquare . Transform.scale v)
  where
    v = Vector.mk 2 3

hScaleUSquare :: (String, BImage.BImage)
hScaleUSquare = ("hScaleUSquare.bmp",  BImage.Generator.uSquare . Transform.hScale 3)

vScaleUSquare :: (String, BImage.BImage)
vScaleUSquare = ("vScaleUSquare.bmp",  BImage.Generator.uSquare . Transform.vScale 3)

uScaleUSquare :: (String, BImage.BImage)
uScaleUSquare = ("uScaleUSquare.bmp",  BImage.Generator.uSquare . Transform.uScale 2)

rotateUSquare :: (String, BImage.BImage)
rotateUSquare = ("rotateUSquare.bmp",  BImage.Generator.uSquare . Transform.rotate (pi/4))
```
produce the following images:
![translateUSquare](/images/translateUSquare.bmp)
![hTranslateUSquare](/images/hTranslateUSquare.bmp)
![vTranslateUSquare](/images/vTranslateUSquare.bmp)
![scaleUSquare](/images/scaleUSquare.bmp)
![hScaleUSquare](/images/hScaleUSquare.bmp)
![vScaleUSquare](/images/vScaleUSquare.bmp)
![uScaleUSquare](/images/uScaleUSquare.bmp)
![rotateUSquare](/images/rotateUSquare.bmp)

,
## Boolean image (aka Region) Algebra
