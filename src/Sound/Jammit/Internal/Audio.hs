{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
module Sound.Jammit.Internal.Audio
( readIMA
, writeWAV
) where

import qualified Data.Conduit as C
import qualified Data.Vector as V
import Data.Int
import Data.Word
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as B
import Data.ByteString.Char8 ()
import System.IO
import GHC.IO.Handle (HandlePosn(..))
import Data.Bits
import Data.Foldable (forM_)
import Control.Monad (unless, forM)

import Control.Monad.ST
import Data.STRef

findChunks :: Integer -> Handle -> IO [(B.ByteString, (HandlePosn, Integer))]
findChunks 0   _ = return []
findChunks len h = do
  ctype <- B.hGet h 4
  clen <- fmap toInteger (readBE h :: IO Word32)
  cstart <- hGetPosn h
  hSeek h RelativeSeek clen
  rest <- findChunks (len - clen - 8) h
  return $ (ctype, (cstart, clen)) : rest

readIMA :: (MonadIO m) => FilePath -> C.Source m (V.Vector (Int16, Int16))
readIMA fp = do
  h <- liftIO $ openBinaryFile fp ReadMode
  "FORM" <- liftIO $ B.hGet h 4
  len <- liftIO (readBE h :: IO Word32)
  "AIFC" <- liftIO $ B.hGet h 4
  chunks <- liftIO $ findChunks (fromIntegral len - 4) h
  frames <- case lookup "COMM" chunks of
    Nothing -> error "readIMA: no COMM chunk"
    Just (comm, _) -> do
      liftIO $ hSetPosn comm
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
  case lookup "SSND" chunks of
    Nothing -> error "readIMA: no SSND chunk"
    Just (ssnd, _) -> do
      liftIO $ hSetPosn ssnd
      0 <- liftIO (readBE h :: IO Word32) -- offset
      0 <- liftIO (readBE h :: IO Word32) -- blocksize
      let go _     _     0         = return ()
          go predL predR remFrames = do
            chunkL <- liftIO $ B.hGet h 34
            chunkR <- liftIO $ B.hGet h 34
            let (predL', vectL) = decodeChunk (predL, chunkL)
                (predR', vectR) = decodeChunk (predR, chunkR)
            V.zip vectL vectR `C.yieldOr` liftIO (hClose h)
            go predL' predR' $ remFrames - 1
      go 0 0 frames
  liftIO $ hClose h

decodeChunk :: (Int16, B.ByteString) -> (Int16, V.Vector Int16)
decodeChunk (initPredictor, chunk) = runST $ do
  predictor <- newSTRef initPredictor
  stepIndex <- newSTRef (fromIntegral (B.index chunk 1) .&. 127 :: Int16)
  step <- readSTRef stepIndex >>= newSTRef . (stepTable V.!) . fromIntegral
  v <- fmap (V.fromList . concat) $ forM [0..31] $ \i -> do
    let d = B.index chunk $ i + 2
        hnb = (fromIntegral d `shiftR` 4) .&. 15 :: Int32
        lnb = fromIntegral d .&. 15 :: Int32
    forM [lnb, hnb] $ \nb -> do
      modifySTRef stepIndex (+ (indexTable V.! fromIntegral nb))
      modifySTRef stepIndex $ \si ->
        if si < 0 then 0 else if si > 88 then 88 else si
      let sign = fromIntegral $ nb .&. 8 :: Int32
          delta = fromIntegral $ nb .&. 7 :: Int32
      thisStep <- readSTRef step
      let diff = sum
            [ thisStep `shiftR` 3
            , if delta .&. 4 /= 0 then thisStep            else 0
            , if delta .&. 2 /= 0 then thisStep `shiftR` 1 else 0
            , if delta .&. 1 /= 0 then thisStep `shiftR` 2 else 0
            ]
      prevPredictor <- readSTRef predictor
      if sign /= 0
        then let
          p = fromIntegral prevPredictor - fromIntegral diff :: Int32
          in writeSTRef predictor $ if p < (-32768) then -32768 else fromIntegral p
        else let
          p = fromIntegral prevPredictor + fromIntegral diff :: Int32
          in writeSTRef predictor $ if p > 32767 then 32767 else fromIntegral p
      readSTRef stepIndex >>= writeSTRef step . (stepTable V.!) . fromIntegral
      readSTRef predictor
  finalPredictor <- readSTRef predictor
  return (finalPredictor, v)

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
  h <- liftIO $ openBinaryFile fp WriteMode
  let startChunk ctype = liftIO $ do
        B.hPut h ctype
        B.hPut h "XXXX" -- filled in later
        hGetPosn h
      endChunk startPosn = liftIO $ do
        endPosn <- hGetPosn h
        case (startPosn, endPosn) of
          (HandlePosn _ start, HandlePosn _ end) -> do
            hSeek h AbsoluteSeek $ start - 4
            writeLE h (fromIntegral $ end - start :: Word32)
            hSeek h AbsoluteSeek end
      chunk ctype f = do
        c <- startChunk ctype
        x <- f
        endChunk c
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
    chunk "data" $ let
      go = C.await >>= \case
        Nothing -> return ()
        Just v  -> do
          forM_ v $ \(l, r) -> liftIO $ do
            writeLE h l
            writeLE h r
          go
      in go
  liftIO $ hClose h

class BE a where
  readBE :: Handle -> IO a

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
  writeLE :: Handle -> a -> IO ()

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
