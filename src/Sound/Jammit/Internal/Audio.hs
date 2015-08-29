{- |
AIFC\/IMA audio decoding functions in this module are ported from
http://sed.free.fr/aifc2wav.html
-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
module Sound.Jammit.Internal.Audio
( readIMA
, writeWAV
, clamp
) where

import           Control.Monad                (liftM2, unless)
import           Control.Monad.IO.Class       (liftIO)
import qualified Data.ByteString              as B
import           Data.ByteString.Char8        () -- for IsString instance
import qualified Data.ByteString.Unsafe       as B
import qualified Data.Conduit                 as C
import qualified Data.Conduit.List            as CL
import qualified Data.Vector.Storable         as V
import           Foreign                      (Int16, Int32, Ptr, Word16,
                                               Word32, Word8, castPtr,
                                               finalizerFree, mallocBytes,
                                               newForeignPtr, shiftL, shiftR)
import           GHC.IO.Handle                (HandlePosn (..))
import qualified System.IO                    as IO
import           System.IO.Unsafe             (unsafePerformIO)

import           Control.Monad.Trans.Resource (MonadResource)
import qualified Data.Conduit.Audio           as A

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

readIMA :: (MonadResource m) => FilePath -> IO (A.AudioSource m Int16)
readIMA fp = do
  let insideChunk h ctype maybeEnd f = do
        here <- liftIO $ IO.hGetPosn h
        chunks <- liftIO $ parseChunksUntil maybeEnd h
        case lookup ctype chunks of
          Nothing -> error $ "readIMA: no chunk of type " ++ show ctype
          Just (start, end) -> do
            liftIO $ IO.hSetPosn start
            x <- f end
            liftIO $ IO.hSetPosn here
            return x
  frames <- IO.withBinaryFile fp IO.ReadMode $ \h -> do
    let chunkBefore = insideChunk h
    "FORM" `chunkBefore` Nothing $ \formEnd -> do
      "AIFC" <- liftIO $ B.hGet h 4
      "COMM" `chunkBefore` Just formEnd $ \_ -> do
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
  let src = C.bracketP
        (IO.openBinaryFile fp IO.ReadMode)
        IO.hClose
        $ \h -> do
          let chunkBefore = insideChunk h
          "FORM" `chunkBefore` Nothing $ \formEnd -> do
            "AIFC" <- liftIO $ B.hGet h 4
            "SSND" `chunkBefore` Just formEnd $ \_ -> do
              0 <- liftIO (readBE h :: IO Word32) -- offset
              0 <- liftIO (readBE h :: IO Word32) -- blocksize
              let go _     _     0         = return ()
                  go predL predR remFrames = do
                    chunkL <- liftIO $ B.hGet h 34
                    chunkR <- liftIO $ B.hGet h 34
                    let (predL', vectL) = decodeChunk (predL, chunkL)
                        (predR', vectR) = decodeChunk (predR, chunkR)
                    C.yield $ A.interleave [vectL, vectR]
                    go predL' predR' $ remFrames - 1
              go 0 0 frames
  return $ A.AudioSource src 44100 2 $ fromIntegral frames

foreign import ccall unsafe "decode_chunk"
  c_decodeChunk :: Ptr Word8 -> Ptr Word8 -> Int16 -> IO Int16

decodeChunk :: (Int16, B.ByteString) -> (Int16, V.Vector Int16)
decodeChunk (initPredictor, chunk) = unsafePerformIO $ do
  B.unsafeUseAsCString chunk $ \cstr -> do
    p <- mallocBytes 128
    lastPredictor <- c_decodeChunk (castPtr cstr) p initPredictor
    fp <- newForeignPtr finalizerFree $ castPtr p
    return (lastPredictor, V.unsafeFromForeignPtr0 fp 64)

clamp :: (Ord a) => (a, a) -> a -> a
clamp (vmin, vmax) v
  | v < vmin  = vmin
  | v > vmax  = vmax
  | otherwise = v

writeWAV :: (MonadResource m) => FilePath -> A.AudioSource m Int16 -> m ()
writeWAV fp (A.AudioSource s r c _) = s C.$$ C.bracketP
  (IO.openBinaryFile fp IO.WriteMode)
  IO.hClose
  (\h -> do
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
        writeLE h (1                            :: Word16) -- 1 is PCM
        writeLE h (fromIntegral c               :: Word16) -- channels
        writeLE h (floor r                      :: Word32) -- sample rate
        writeLE h (floor r * fromIntegral c * 2 :: Word32) -- avg. bytes per second = rate * block align
        writeLE h (fromIntegral c * 2           :: Word16) -- block align = chans * (bps / 8)
        writeLE h (16                           :: Word16) -- bits per sample
      chunk "data" $ CL.mapM_ $ \v -> liftIO $ do
        V.forM_ v $ writeLE h
  )

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
