module Sound.Jammit.Internal.Image
( partsToPages
, jpegsToPDF
) where

import qualified Codec.Picture as P
import Codec.Picture.Types (convertImage)
import Control.Monad (forM_, replicateM)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import qualified Graphics.PDF as PDF
import qualified Data.ByteString.Lazy as BL
import qualified Data.Conduit as C
import Data.Maybe (catMaybes)
import qualified Data.Vector.Storable as V

import Sound.Jammit.Internal.TempIO

loadPNG :: FilePath -> IO (P.Image P.PixelRGB8)
loadPNG fp = do
  Right dyn <- P.readImage fp
  return $ P.convertRGB8 dyn

pngChunks :: (MonadIO m) =>
  Int -> [FilePath] -> C.Source m (P.Image P.PixelRGB8)
pngChunks h fps = let
  raw :: (MonadIO m) => C.Source m (P.Image P.PixelRGB8)
  raw = mapM_ (\fp -> liftIO (loadPNG fp) >>= C.yield) fps
  chunk :: (Monad m) =>
    C.Conduit (P.Image P.PixelRGB8) m (P.Image P.PixelRGB8)
  chunk = C.await >>= \x -> case x of
    Nothing   -> return ()
    Just page -> case span (\c -> P.imageHeight c == h) $ vertSplit h page of
      (full, []  ) -> mapM_ C.yield full >> chunk
      (full, part) -> mapM_ C.yield full >> C.await >>= \y -> case y of
        Nothing    -> mapM_ C.yield part
        Just page' -> C.leftover (vertConcat $ part ++ [page']) >> chunk
  in raw C.=$= chunk

chunksToPages :: (Monad m) =>
  Int -> C.Conduit [P.Image P.PixelRGB8] m (P.Image P.PixelRGB8)
chunksToPages n = fmap catMaybes (replicateM n C.await) >>= \systems -> case systems of
  [] -> return ()
  _  -> C.yield (vertConcat $ concat systems) >> chunksToPages n

sinkJPEG :: C.Sink (P.Image P.PixelRGB8) TempIO [FilePath]
sinkJPEG = go [] where
  go jpegs = C.await >>= \x -> case x of
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

saveJPEG :: FilePath -> P.Image P.PixelRGB8 -> IO ()
saveJPEG fp img = BL.writeFile fp $ P.encodeJpegAtQuality 100 $ convertImage img

vertConcat :: [P.Image P.PixelRGB8] -> P.Image P.PixelRGB8
vertConcat [] = P.Image 0 0 V.empty
-- efficient version: all images have same width, just concat vectors
vertConcat allimgs@(img : imgs)
  | all (\i -> P.imageWidth i == P.imageWidth img) imgs
  = P.Image
    { P.imageWidth  = P.imageWidth img
    , P.imageHeight = sum $ map P.imageHeight allimgs
    , P.imageData   = V.concat $ map P.imageData allimgs
    }
-- this algorithm is probably not needed
vertConcat imgs = P.generateImage f w h where
  w = foldr max 0 $ map P.imageWidth imgs
  h = sum $ map P.imageHeight imgs
  f = go imgs
  empty = P.PixelRGB8 0 0 0
  go [] _ _ = empty
  go (i : is) x y = if y < P.imageHeight i
    then if x < P.imageWidth i
      then P.pixelAt i x y
      else empty
    else go is x $ y - P.imageHeight i

vertSplit :: Int -> P.Image P.PixelRGB8 -> [P.Image P.PixelRGB8]
vertSplit h img = if P.imageHeight img <= h
  then [img]
  else let
    chunkSize = P.pixelBaseIndex img 0 h
    first = P.Image
      { P.imageWidth  = P.imageWidth img
      , P.imageHeight = h
      , P.imageData   = V.take chunkSize $ P.imageData img
      }
    rest = P.Image
      { P.imageWidth  = P.imageWidth img
      , P.imageHeight = P.imageHeight img - h
      , P.imageData   = V.drop chunkSize $ P.imageData img
      }
    in first : vertSplit h rest

imagePage :: PDF.JpegFile -> PDF.PDF ()
imagePage jpeg = do
  let (w, h) = PDF.jpegBounds jpeg
  page <- PDF.addPage $ Just $ PDF.PDFRect 0 0 w h
  ref <- PDF.createPDFJpeg jpeg
  PDF.drawWithPage page $ PDF.drawXObject ref

jpegsToPDF :: [FilePath] -> FilePath -> IO ()
jpegsToPDF jpegs pdf = do
  Right js <- fmap sequence $ mapM PDF.readJpegFile jpegs
  PDF.runPdf pdf PDF.standardDocInfo (PDF.PDFRect 0 0 600 400) $
    forM_ js imagePage
