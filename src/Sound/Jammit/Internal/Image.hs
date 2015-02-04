module Sound.Jammit.Internal.Image
( loadPNG, saveJPEG
, vertConcat, vertSplit
, jpegsToPDF
) where

import qualified Codec.Picture as P
import Codec.Picture.Types (convertImage, dropTransparency)
import Control.Monad
import qualified Graphics.PDF as PDF
import qualified Data.ByteString.Lazy as BL

loadPNG :: FilePath -> IO (P.Image P.PixelRGBA8)
loadPNG fp = do
  Right dyn <- P.readImage fp
  case dyn of
    P.ImageRGBA8 i -> return i
    _              -> error "loadPNG: pixels aren't RGBA8"

saveJPEG :: FilePath -> P.Image P.PixelRGBA8 -> IO ()
saveJPEG fp img = BL.writeFile fp $ P.encodeJpegAtQuality 100 $
  convertImage $ P.pixelMap dropTransparency img

vertConcat :: [P.Image P.PixelRGBA8] -> P.Image P.PixelRGBA8
vertConcat imgs = P.generateImage f w h where
  w = foldr max 0 $ map P.imageWidth imgs
  h = sum $ map P.imageHeight imgs
  f = go imgs
  empty = P.PixelRGBA8 0 0 0 0
  go [] _ _ = empty
  go (i : is) x y = if y < P.imageHeight i
    then if x < P.imageWidth i
      then P.pixelAt i x y
      else empty
    else go is x $ y - P.imageHeight i

vertSplit :: Int -> P.Image P.PixelRGBA8 -> [P.Image P.PixelRGBA8]
vertSplit h img = map f $ takeWhile (< P.imageHeight img) [0, h ..] where
  f yoff = P.generateImage
    (g yoff)
    (P.imageWidth img)
    (min h $ P.imageHeight img - yoff)
  g yoff x y = P.pixelAt img x $ y + yoff

imagePage :: PDF.JpegFile -> PDF.PDF ()
imagePage jpeg = do
  let (w, h) = PDF.jpegBounds jpeg
  page <- PDF.addPage $ Just $ PDF.PDFRect 0 0 (round w) (round h)
  ref <- PDF.createPDFJpeg jpeg
  PDF.drawWithPage page $ PDF.drawXObject ref

jpegsToPDF :: [FilePath] -> FilePath -> IO ()
jpegsToPDF jpegs pdf = do
  Right js <- fmap sequence $ mapM PDF.readJpegFile jpegs
  PDF.runPdf pdf PDF.standardDocInfo (PDF.PDFRect 0 0 600 400) $
    forM_ js imagePage
