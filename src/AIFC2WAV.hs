{-# LANGUAGE ForeignFunctionInterface #-}
module AIFC2WAV where

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Array

foreign import ccall "aifc2wav_main" aifc2wav_main
  :: CInt -> Ptr (Ptr CChar) -> IO CInt

aifcToWav :: FilePath -> FilePath -> IO ()
aifcToWav aifc wav =
  withCString "aifc2wav" $ \progC ->
  withCString aifc $ \aifcC ->
  withCString wav $ \wavC ->
  withArrayLen [progC, aifcC, wavC] $ \n v -> do
  code <- aifc2wav_main (fromIntegral n) v
  case code of
    0 -> return ()
    _ -> error $ "aifcToWav: returned " ++ show code
