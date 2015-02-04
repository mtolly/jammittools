{-# LANGUAGE LambdaCase #-}
module Sound.Jammit.Internal.Image
( partsToPages
, jpegsToPDF
) where

import qualified Codec.Picture as P
import Codec.Picture.Types (convertImage, dropTransparency)
import Control.Monad (forM_, replicateM)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import qualified Graphics.PDF as PDF
import qualified Data.ByteString.Lazy as BL
import qualified Data.Conduit as C
import Data.Maybe (catMaybes)

import Sound.Jammit.Internal.TempFile

loadPNG :: FilePath -> IO (P.Image P.PixelRGBA8)
loadPNG fp = do
  Right dyn <- P.readImage fp
  case dyn of
    P.ImageRGBA8 i -> return i
    _              -> error "loadPNG: pixels aren't RGBA8"

pngChunks :: (MonadIO m) =>
  Int -> [FilePath] -> C.Source m (P.Image P.PixelRGBA8)
pngChunks h fps = let
  raw :: (MonadIO m) => C.Source m (P.Image P.PixelRGBA8)
  raw = mapM_ (\fp -> liftIO (loadPNG fp) >>= C.yield) fps
  chunk :: (Monad m) =>
    C.Conduit (P.Image P.PixelRGBA8) m (P.Image P.PixelRGBA8)
  chunk = C.await >>= \case
    Nothing   -> return ()
    Just page -> case span (\c -> P.imageHeight c == h) $ vertSplit h page of
      (full, []  ) -> mapM_ C.yield full >> chunk
      (full, part) -> mapM_ C.yield full >> C.await >>= \case
        Nothing    -> mapM_ C.yield part
        Just page' -> C.leftover (vertConcat $ part ++ [page']) >> chunk
  in raw C.=$= chunk

chunksToPages :: (Monad m) =>
  Int -> C.Conduit [P.Image P.PixelRGBA8] m (P.Image P.PixelRGBA8)
chunksToPages n = fmap catMaybes (replicateM n C.await) >>= \case
  [] -> return ()
  systems -> C.yield (vertConcat $ concat systems) >> chunksToPages n

sinkJPEG :: C.Sink (P.Image P.PixelRGBA8) TempIO [FilePath]
sinkJPEG = go [] where
  go jpegs = C.await >>= \case
    Nothing -> return jpegs
    Just img -> do
      jpeg <- lift $ newTempFile "page.jpg"
      liftIO $ saveJPEG jpeg img
      go $ jpegs ++ [jpeg]

partsToPages
  :: [([FilePath], Integer)] -- ^ [(images, system height)]
  -> Int -- ^ systems per page
  -> TempIO [FilePath]
partsToPages parts n = let
  sources = map (\(imgs, h) -> pngChunks (fromIntegral h) imgs) parts
  in C.sequenceSources sources C.$$ chunksToPages n C.=$= sinkJPEG

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
