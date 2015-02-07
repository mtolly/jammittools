{- |
AIFC\/IMA audio decoding functions in this module are ported from
http://sed.free.fr/aifc2wav.html
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
module Sound.Jammit.Internal.Audio
( readIMA
, writeWAV
) where

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Vector as V
import Data.Int (Int16, Int32)
import Data.Word (Word16, Word32)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as B
import Data.ByteString.Char8 () -- for IsString instance
import qualified System.IO as IO
import GHC.IO.Handle (HandlePosn(..))
import Data.Bits (shiftL, shiftR, (.&.))
import Data.Foldable (forM_)
import Control.Monad (unless, forM, liftM2)
import Control.Monad.ST (runST)
import Data.STRef (newSTRef, readSTRef, modifySTRef)

parseChunk :: IO.Handle -> IO (B.ByteString, (HandlePosn, HandlePosn))
parseChunk h = do
  ctype <- B.hGet h 4
  clen <- fmap toInteger (readBE h :: IO Word32)
  startPosn <- IO.hGetPosn h
  IO.hSeek h IO.RelativeSeek clen
  endPosn <- IO.hGetPosn h
  return (ctype, (startPosn, endPosn))

parseChunksUntil
  :: Maybe HandlePosn -> IO.Handle -> IO [(B.ByteString, (HandlePosn, HandlePosn))]
parseChunksUntil maybeEnd h = do
  eof <- IO.hIsEOF h
  HandlePosn _ here <- IO.hGetPosn h
  let pastEnd = case maybeEnd of
        Nothing                 -> False
        Just (HandlePosn _ end) -> end <= here
  if eof || pastEnd
    then return []
    else liftM2 (:) (parseChunk h) (parseChunksUntil maybeEnd h)

readIMA :: (MonadIO m) => FilePath -> C.Source m (V.Vector (Int16, Int16))
readIMA fp = do
  h <- liftIO $ IO.openBinaryFile fp IO.ReadMode
  let ctype `chunkBefore` maybeEnd = \f -> do
        here <- liftIO $ IO.hGetPosn h
        chunks <- liftIO $ parseChunksUntil maybeEnd h
        case lookup ctype chunks of
          Nothing -> error $ "readIMA: no chunk of type " ++ show ctype
          Just (start, end) -> do
            liftIO $ IO.hSetPosn start
            x <- f end
            liftIO $ IO.hSetPosn here
            return x
  "FORM" `chunkBefore` Nothing $ \formEnd -> do
    "AIFC" <- liftIO $ B.hGet h 4
    frames <- "COMM" `chunkBefore` Just formEnd $ \_ -> do
      2 <- liftIO (readBE h :: IO Word16) -- channels
      frames <- liftIO (readBE h :: IO Word32) -- number of chunk pairs
      bits <- liftIO (readBE h :: IO Word16) -- bits per sample, 0 means 16?
      unless (bits `elem` [0, 16]) $ error "readIMA: bits per sample not 16 or 0"
      -- next 10 bytes are sample rate as long float
      -- for now we just compare to the known 44100
      0x400eac44 <- liftIO (readBE h :: IO Word32)
      0 <- liftIO (readBE h :: IO Word32)
      0 <- liftIO (readBE h :: IO Word16)
      "ima4" <- liftIO $ B.hGet h 4
      return frames
    "SSND" `chunkBefore` Just formEnd $ \_ -> do
      0 <- liftIO (readBE h :: IO Word32) -- offset
      0 <- liftIO (readBE h :: IO Word32) -- blocksize
      let go _     _     0         = return ()
          go predL predR remFrames = do
            chunkL <- liftIO $ B.hGet h 34
            chunkR <- liftIO $ B.hGet h 34
            let (predL', vectL) = decodeChunk (predL, chunkL)
                (predR', vectR) = decodeChunk (predR, chunkR)
            V.zip vectL vectR `C.yieldOr` liftIO (IO.hClose h)
            go predL' predR' $ remFrames - 1
      go 0 0 frames
  liftIO $ IO.hClose h

decodeChunk :: (Int16, B.ByteString) -> (Int16, V.Vector Int16)
decodeChunk (initPredictor, chunk) = runST $ do
  predictor <- newSTRef initPredictor
  stepIndex <- newSTRef (fromIntegral (B.index chunk 1) .&. 127 :: Int16)
  let step = fmap (\si -> stepTable V.! fromIntegral si) $ readSTRef stepIndex
  v <- fmap (V.fromList . concat) $ forM (B.unpack $ B.drop 2 chunk) $ \d -> do
    let hnb = fromIntegral $ d `shiftR` 4 :: Int32
        lnb = fromIntegral $ d .&. 15     :: Int32
    forM [lnb, hnb] $ \nb -> do
      thisStep <- step
      let sign  = nb .&. 8
          diff = sum
            [ thisStep `shiftR` 3
            , if nb .&. 4 /= 0 then thisStep            else 0
            , if nb .&. 2 /= 0 then thisStep `shiftR` 1 else 0
            , if nb .&. 1 /= 0 then thisStep `shiftR` 2 else 0
            ]
      modifySTRef predictor $ \old -> let
        op = if sign /= 0 then (-) else (+)
        new = fromIntegral old `op` fromIntegral diff :: Int32
        in fromIntegral $ clamp (-32768, 32767) new
      modifySTRef stepIndex $ \old ->
        clamp (0, 88) $ old + (indexTable V.! fromIntegral nb)
      readSTRef predictor
  finalPredictor <- readSTRef predictor
  return (finalPredictor, v)

clamp :: (Ord a) => (a, a) -> a -> a
clamp (vmin, vmax) v
  | v < vmin  = vmin
  | v > vmax  = vmax
  | otherwise = v

indexTable :: V.Vector Int16
indexTable = V.fromList
  [ -1, -1, -1, -1, 2, 4, 6, 8
  , -1, -1, -1, -1, 2, 4, 6, 8
  ]

stepTable :: V.Vector Int16
stepTable = V.fromList
  [ 7, 8, 9, 10, 11, 12, 13, 14, 16, 17
  , 19, 21, 23, 25, 28, 31, 34, 37, 41, 45
  , 50, 55, 60, 66, 73, 80, 88, 97, 107, 118
  , 130, 143, 157, 173, 190, 209, 230, 253, 279, 307
  , 337, 371, 408, 449, 494, 544, 598, 658, 724, 796
  , 876, 963, 1060, 1166, 1282, 1411, 1552, 1707, 1878, 2066
  , 2272, 2499, 2749, 3024, 3327, 3660, 4026, 4428, 4871, 5358
  , 5894, 6484, 7132, 7845, 8630, 9493, 10442, 11487, 12635, 13899
  , 15289, 16818, 18500, 20350, 22385, 24623, 27086, 29794, 32767
  ]

writeWAV :: (MonadIO m) => FilePath -> C.Sink (V.Vector (Int16, Int16)) m ()
writeWAV fp = do
  h <- liftIO $ IO.openBinaryFile fp IO.WriteMode
  let chunk ctype f = do
        let getPosn = liftIO $ IO.hGetPosn h
        liftIO $ B.hPut h ctype
        lenPosn <- getPosn
        liftIO $ B.hPut h $ B.pack [0xDE, 0xAD, 0xBE, 0xEF] -- filled in later
        HandlePosn _ start <- getPosn
        x <- f
        endPosn@(HandlePosn _ end) <- getPosn
        liftIO $ do
          IO.hSetPosn lenPosn
          writeLE h (fromIntegral $ end - start :: Word32)
          IO.hSetPosn endPosn
        return x
  chunk "RIFF" $ do
    liftIO $ B.hPut h "WAVE"
    chunk "fmt " $ liftIO $ do
      writeLE h (1      :: Word16) -- 1 is PCM
      writeLE h (2      :: Word16) -- channels
      writeLE h (44100  :: Word32) -- sample rate
      writeLE h (176400 :: Word32) -- avg. bytes per second = rate * block align
      writeLE h (4      :: Word16) -- block align = chans * (bps / 8)
      writeLE h (16     :: Word16) -- bits per sample
    chunk "data" $ CL.mapM_ $ \v -> liftIO $ do
      forM_ v $ \(l, r) -> do
        writeLE h l
        writeLE h r
  liftIO $ IO.hClose h

class BE a where
  readBE :: IO.Handle -> IO a

instance BE Word32 where
  readBE h = do
    [a, b, c, d] <- fmap B.unpack $ B.hGet h 4
    return $ sum
      [ fromIntegral a `shiftL` 24
      , fromIntegral b `shiftL` 16
      , fromIntegral c `shiftL` 8
      , fromIntegral d
      ]

instance BE Int32 where
  readBE h = fmap fromIntegral (readBE h :: IO Word32)

instance BE Word16 where
  readBE h = do
    [a, b] <- fmap B.unpack $ B.hGet h 2
    return $ sum
      [ fromIntegral a `shiftL` 8
      , fromIntegral b
      ]

instance BE Int16 where
  readBE h = fmap fromIntegral (readBE h :: IO Word16)

class LE a where
  writeLE :: IO.Handle -> a -> IO ()

instance LE Word32 where
  writeLE h w = B.hPut h $ B.pack [a, b, c, d] where
    a = fromIntegral w
    b = fromIntegral $ w `shiftR` 8
    c = fromIntegral $ w `shiftR` 16
    d = fromIntegral $ w `shiftR` 24

instance LE Word16 where
  writeLE h w = B.hPut h $ B.pack [a, b] where
    a = fromIntegral w
    b = fromIntegral $ w `shiftR` 8

instance LE Int32 where
  writeLE h w = writeLE h (fromIntegral w :: Word32)

instance LE Int16 where
  writeLE h w = writeLE h (fromIntegral w :: Word16)
