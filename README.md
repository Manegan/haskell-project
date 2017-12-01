# fimage-squeleton
Functional Image (squeleton)

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

## Boolean image (aka Region) Algebra
