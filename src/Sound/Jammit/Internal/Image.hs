module Sound.Jammit.Internal.Image
( partsToPages
, jpegsToPDF
) where

import qualified Codec.Picture                as P
import           Codec.Picture.Types          (convertImage)
import           Control.Monad                (forM_, replicateM)
import           Control.Monad.IO.Class       (MonadIO(..))
import qualified Data.ByteString.Lazy         as BL
import qualified Data.ByteString         as B
import           Data.Conduit                 ((.|))
import           Data.Conduit.List (consume)
import qualified Data.Conduit                 as C
import           Data.Maybe                   (catMaybes)
import qualified Data.Vector.Storable         as V
import Foreign
import Foreign.C
import Control.Exception (bracket)
import Codec.Picture.Jpg (encodeJpegAtQuality)

data PDFInfo
data PDFDoc
data PDFObject

foreign import ccall unsafe "pdf_create"
  pdf_create :: CInt -> CInt -> Ptr PDFInfo -> IO (Ptr PDFDoc)

foreign import ccall unsafe "pdf_create_nostruct"
  pdf_create_nostruct :: CInt -> CInt
    -> CString -> CString -> CString -> CString -> CString -> CString
    -> IO (Ptr PDFDoc)

foreign import ccall unsafe "pdf_append_page"
  pdf_append_page :: Ptr PDFDoc -> IO (Ptr PDFObject)

foreign import ccall unsafe "pdf_page_set_size"
  pdf_page_set_size :: Ptr PDFDoc -> Ptr PDFObject -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "pdf_add_jpeg"
  pdf_add_jpeg :: Ptr PDFDoc -> Ptr PDFObject -> CInt -> CInt -> CInt -> CInt -> CString -> IO CInt

foreign import ccall unsafe "pdf_add_jpeg_direct"
  pdf_add_jpeg_direct
    :: Ptr PDFDoc
    -> Ptr PDFObject
    -> CInt -> CInt
    -> CInt -> CInt
    -> CInt -> CInt
    -> Ptr Word8 -> CSize
    -> IO CInt

foreign import ccall unsafe "pdf_save"
  pdf_save :: Ptr PDFDoc -> CString -> IO CInt

foreign import ccall unsafe "pdf_destroy"
  pdf_destroy :: Ptr PDFDoc -> IO ()

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
  in C.runConduit $ C.sequenceSources sources .| chunksToPages n .| consume

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

jpegsToPDF :: [P.Image P.PixelRGB8] -> FilePath -> IO ()
jpegsToPDF [] _ = return () -- Buddy Rich "Love for Sale" Kick channel
jpegsToPDF jpegs pdf = let
  inch i = round $ (i :: Rational) * 72
  pageWidth = inch 8.5
  pageHeight = inch 11
  in withCString "" $ \mt -> do
    bracket (pdf_create_nostruct pageWidth pageHeight mt mt mt mt mt mt) pdf_destroy $ \doc -> do
      let check fn = fn >>= \ret -> case ret of 0 -> return (); e -> error $ show e
      forM_ jpegs $ \jpeg@(P.Image w h _) -> do
        let thisHeight = round $ (toRational h / toRational w) * toRational pageWidth
        page <- pdf_append_page doc
        check $ pdf_page_set_size doc page pageWidth thisHeight
        let bs = encodeJpegAtQuality 100 $ convertImage jpeg
        check $ B.useAsCStringLen (BL.toStrict bs) $ \(p, len) -> pdf_add_jpeg_direct
          doc page
          (fromIntegral w) (fromIntegral h)
          0 0
          pageWidth thisHeight
          (castPtr p) (fromIntegral len)
      -- TODO make non-ascii filename work on Windows with getShortPathName
      check $ withCString pdf $ pdf_save doc
