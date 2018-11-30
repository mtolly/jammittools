module Sound.Jammit.Internal.Image
( partsToPages
, pagesToPDF
) where

import qualified Codec.Picture             as P
import           Codec.Picture.Types       (promoteImage)
import           Control.Monad             (replicateM)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import qualified Data.ByteString.Lazy      as BL
import           Data.Conduit              ((.|))
import qualified Data.Conduit              as C
import qualified Data.Conduit.List         as CL
import           Data.Maybe                (catMaybes)
import qualified Data.Vector.Storable      as V
import qualified Graphics.Rasterific       as R

loadPNG :: FilePath -> IO (P.Image P.PixelRGB8)
loadPNG fp = do
  Right dyn <- P.readImage fp
  return $ P.convertRGB8 dyn

pngChunks :: (MonadIO m) =>
  Int -> [FilePath] -> C.ConduitT () (P.Image P.PixelRGB8) m ()
pngChunks h fps = let
  raw :: (MonadIO m) => C.ConduitT () (P.Image P.PixelRGB8) m ()
  raw = mapM_ (\fp -> liftIO (loadPNG fp) >>= C.yield) fps
  chunk :: (Monad m) =>
    C.ConduitT (P.Image P.PixelRGB8) (P.Image P.PixelRGB8) m ()
  chunk = C.await >>= \x -> case x of
    Nothing   -> return ()
    Just page -> case span (\c -> P.imageHeight c == h) $ vertSplit h page of
      (full, []  ) -> mapM_ C.yield full >> chunk
      (full, part) -> mapM_ C.yield full >> C.await >>= \y -> case y of
        Nothing    -> mapM_ C.yield part
        Just page' -> C.leftover (vertConcat $ part ++ [page']) >> chunk
  in raw .| chunk

chunksToPages :: (Monad m) =>
  Int -> C.ConduitT [P.Image P.PixelRGB8] (P.Image P.PixelRGB8) m ()
chunksToPages n = fmap catMaybes (replicateM n C.await) >>= \systems -> case systems of
  [] -> return ()
  _  -> C.yield (vertConcat $ concat systems) >> chunksToPages n

partsToPages
  :: [([FilePath], Integer)] -- ^ [(images, system height)]
  -> Int -- ^ systems per page
  -> IO [P.Image P.PixelRGB8]
partsToPages parts n = let
  sources = map (\(imgs, h) -> pngChunks (fromIntegral h) imgs) parts
  in C.runConduit $ C.sequenceSources sources .| chunksToPages n .| CL.consume

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

pagesToPDF :: FilePath -> [P.Image P.PixelRGB8] -> IO ()
pagesToPDF _    []           = return ()
pagesToPDF fout imgs@(i : _) = BL.writeFile fout $ R.renderDrawingsAtDpiToPDF
  (P.imageWidth  i)
  (P.imageHeight i)
  72
  [ R.drawImage (promoteImage img) 0 (R.V2 0 0) | img <- imgs ]
