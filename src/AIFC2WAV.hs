{-# LANGUAGE ForeignFunctionInterface #-}
module AIFC2WAV
( aifcToWav
) where

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Array
import System.IO.Temp (openTempFile)
import System.IO (hClose)

foreign import ccall "aifc2wav_main" aifc2wav_main
  :: CInt -> Ptr (Ptr CChar) -> IO CInt

foreign import ccall "reset_predictors" reset_predictors
  :: IO ()

aifcToWav :: FilePath -> FilePath -> IO FilePath
aifcToWav aifc tempdir = do
  (wav, h) <- openTempFile tempdir "aifcToWav.wav"
  hClose h
  withCString "aifc2wav" $ \progC ->
    withCString aifc $ \aifcC ->
      withCString wav $ \wavC ->
        withArrayLen [progC, aifcC, wavC] $ \n v -> do
        reset_predictors
        code <- aifc2wav_main (fromIntegral n) v
        if code == 0
          then return wav
          else error $ "aifcToWav: returned " ++ show code
