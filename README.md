# fimage-squeleton
Functional Image (squeleton)


```haskell
import qualified Data.FImage.BMP    as BMP
import qualified Data.FImage.BImage.Generator as BImage.Generator
import qualified Data.FImage.View   as View
import qualified Data.FImage.Window as Window

main :: IO ()
main = do
  let filename = "wedgeAnnulus.bmp"
  let image    = BImage.Generator.wedgeAnnulus 0.27 7
  let view     = View.mk0 3.0 3.0
  let window   = Window.mk 512 512
  let bmp      = BMP.bmp window view image
  BMP.write filename bmp
```
