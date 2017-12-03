module Data.FImage.BMP
(
  bmp
, write
)
where

    import qualified Data.Foldable as F

    import qualified Data.ByteString as ByteString
    import qualified Data.Word       as Word

    import qualified Codec.BMP as BMP

    import qualified Data.FImage.Geometry.Point as Point
    import qualified Data.FImage.BImage         as BImage
    import qualified Data.FImage.Interval       as Interval
    import qualified Data.FImage.Render         as Render
    import qualified Data.FImage.View           as View
    import qualified Data.FImage.Window         as Window

    -- | Create a BMP.BMP image from a boolean iage, a window and a view.
    bmp :: Window.Window -> View.View -> BImage.BImage -> BMP.BMP
    bmp w v f = bmp' ww wh w8s
      where
        xMin = View.xMin v
        yMin = View.yMin v
        xMax = View.xMax v
        yMax = View.yMax v

        ix = Interval.mk xMin xMax
        iy = Interval.mk yMin yMax

        ww = Window.width  w
        wh = Window.height w

        w8s = F.concatMap Render.render [f p | y <- [0..wh-1]
                                             , x <- [0..ww-1]
                                             , let rx = Interval.locate ww x ix
                                             , let ry = Interval.locate wh y iy
                                             , let p  = Point.mk rx ry]

    -- | bmp helper function
    bmp' :: Int -> Int -> [Word.Word8] -> BMP.BMP
    bmp' w h = BMP.packRGBA32ToBMP w h . ByteString.pack

    -- | write file to disk
    write :: FilePath -> BMP.BMP -> IO ()
    write = BMP.writeBMP
