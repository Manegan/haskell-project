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

The following functions (see testing program `FImage` in `src`):
```haskell
hStrip :: (String, BImage.BImage)
hStrip = ("hStrip.bmp",   BImage.Generator.hStrip 2)

uHStrip :: (String, BImage.BImage)
uHStrip = ("uHStrip.bmp",  BImage.Generator.uHStrip)

vStrip :: (String, BImage.BImage)
vStrip = ("vStrip.bmp",   BImage.Generator.vStrip 2)

uVStrip :: (String, BImage.BImage)
uVStrip = ("uHStrip.bmp",  BImage.Generator.uVStrip)

cross :: (String, BImage.BImage)
cross = ("cross.bmp",  BImage.Generator.cross 2)

checker :: (String, BImage.BImage)
checker = ("checker.bmp",  BImage.Generator.checker)

altRings :: (String, BImage.BImage)
altRings = ("altRings.bmp", BImage.Generator.altRings)

disk :: (String, BImage.BImage)
disk = ("disk.bmp",     BImage.Generator.disk 2)

uDisk :: (String, BImage.BImage)
uDisk = ("uDisk.bmp",    BImage.Generator.uDisk)

square :: (String, BImage.BImage)
square = ("square.bmp",   BImage.Generator.square 2)

uSquare :: (String, BImage.BImage)
uSquare = ("uSquare.bmp",  BImage.Generator.uSquare)

polarChecker :: (String, BImage.BImage)
polarChecker = ("polarChecker.bmp", BImage.Generator.polarChecker 7)
```

produce the following bitmaps (display parameters `View.mk0 6 6` and
`Window.mk 256 256`):

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

Spatial transforms are simply defined as space-to-space functions
(transforms map points to points)
The type `Transform` is defined in module `Data.FImage.Transform` as follows:

```haskell
import qualified Data.FImage.Geometry.Point  as Point
```

**Define the following functions in `Data.FImage.Transform`**:

```haskell
type Transform = Point.Point -> Point.Point

-- | Translate according to a given vector (dx, dy).
translate :: Vector.Vector -> Transform

-- | Horizontal translate according to a given float.
hTranslate :: Float -> Transform

-- | Vertical translate according to a given float.
vTranslate :: Float -> Transform

-- | Scale according to a given vector (dx, dy).
scale :: Vector.Vector -> Transform

-- | Horizontal translate according to a given float.
hScale :: Float -> Transform

-- | Vertical translate according to a given float.
vScale :: Float -> Transform

-- | Scale according to a given vector (dx, dy) with dx = dy.
uScale :: Float -> Transform

-- | Rotate acoording to a given angle t.
-- For a given point (x, y) the roated point is defined by
-- (x cos(t) - y sint(t), x sin(t) + y cos(t)).
rotate :: Float -> Transform
```

The following functions (see testing program `FImage` in `src`):

```haskell
translateUSquareTransform :: (String, BImage.BImage)
translateUSquareTransform = ("translateUSquareTransform.bmp",  BImage.Generator.uSquare . Transform.translate v)
  where
    v = Vector.mk 2 3

hTranslateUSquareTransform :: (String, BImage.BImage)
hTranslateUSquareTransform =  ("hTranslateUSquareTransform.bmp",  BImage.Generator.uSquare . Transform.hTranslate 3)

vTranslateUSquareTransform :: (String, BImage.BImage)
vTranslateUSquareTransform =  ("vTranslateUSquareTransform.bmp",  BImage.Generator.uSquare . Transform.vTranslate 3)

scaleUSquareTransform :: (String, BImage.BImage)
scaleUSquareTransform = ("scaleUSquareTransform.bmp",  BImage.Generator.uSquare . Transform.scale v)
  where
    v = Vector.mk 2 3

hScaleUSquareTransform :: (String, BImage.BImage)
hScaleUSquareTransform = ("hScaleUSquareTransform.bmp",  BImage.Generator.uSquare . Transform.hScale 3)

vScaleUSquareTransform :: (String, BImage.BImage)
vScaleUSquareTransform = ("vScaleUSquareTransform.bmp",  BImage.Generator.uSquare . Transform.vScale 3)

uScaleUSquareTransform :: (String, BImage.BImage)
uScaleUSquareTransform = ("uScaleUSquareTransform.bmp",  BImage.Generator.uSquare . Transform.uScale 2)

rotateUSquareTransform :: (String, BImage.BImage)
rotateUSquareTransform = ("rotateUSquareTransform.bmp",  BImage.Generator.uSquare . Transform.rotate (pi/4))
```
produce the following bitmaps (display parameters `View.mk0 6 6` and
`Window.mk 256 256`):

![translateUSquareTransform](/images/translateUSquareTransform.bmp)
![hTranslateUSquareTransform](/images/hTranslateUSquareTransform.bmp)
![vTranslateUSquareTransform](/images/vTranslateUSquareTransform.bmp)
![scaleUSquareTransform](/images/scaleUSquareTransform.bmp)
![hScaleUSquareTransform](/images/hScaleUSquareTransform.bmp)
![vScaleUSquareTransform](/images/vScaleUSquareTransform.bmp)
![uScaleUSquareTransform](/images/uScaleUSquareTransform.bmp)
![rotateUSquareTransform](/images/rotateUSquareTransform.bmp)

### Filtering

The following functions (see testing program `FImage` in `src`):
```haskell
translateUSquareFilter :: (String, BImage.BImage)
translateUSquareFilter = ("translateUSquareFilter.bmp",  BImage.Generator.uSquare . Filter.translate v)
  where
    v = Vector.mk 2 3

hTranslateUSquareFilter :: (String, BImage.BImage)
hTranslateUSquareFilter =  ("hTranslateUSquareFilter.bmp",  BImage.Generator.uSquare . Filter.hTranslate 3)

vTranslateUSquareFilter :: (String, BImage.BImage)
vTranslateUSquareFilter =  ("vTranslateUSquareFilter.bmp",  BImage.Generator.uSquare . Filter.vTranslate 3)

scaleUSquareFilter :: (String, BImage.BImage)
scaleUSquareFilter = ("scaleUSquareFilter.bmp",  BImage.Generator.uSquare . Filter.scale v)
  where
    v = Vector.mk 2 3

hScaleUSquareFilter :: (String, BImage.BImage)
hScaleUSquareFilter = ("hScaleUSquareFilter.bmp",  BImage.Generator.uSquare . Filter.hScale 3)

vScaleUSquareFilter :: (String, BImage.BImage)
vScaleUSquareFilter = ("vScaleUSquareFilter.bmp",  BImage.Generator.uSquare . Filter.vScale 3)

uScaleUSquareFilter :: (String, BImage.BImage)
uScaleUSquareFilter = ("uScaleUSquareFilter.bmp",  BImage.Generator.uSquare . Filter.uScale 2)

rotateUSquareFilter :: (String, BImage.BImage)
rotateUSquareFilter = ("rotateUSquareFilter.bmp",  BImage.Generator.uSquare . Filter.rotate (pi/4))
```
produce the following images:

![translateUSquareFilter](/images/translateUSquareFilter.bmp)
![hTranslateUSquareFilter](/images/hTranslateUSquareFilter.bmp)
![vTranslateUSquareFilter](/images/vTranslateUSquareFilter.bmp)
![scaleUSquareFilter](/images/scaleUSquareFilter.bmp)
![hScaleUSquareFilter](/images/hScaleUSquareFilter.bmp)
![vScaleUSquareFilter](/images/vScaleUSquareFilter.bmp)
![uScaleUSquareFilter](/images/uScaleUSquareFilter.bmp)
![rotateUSquareFilter](/images/rotateUSquareFilter.bmp)

## Boolean image (aka Region) Algebra
