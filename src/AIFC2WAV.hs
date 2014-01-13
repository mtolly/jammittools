{-# LANGUAGE ForeignFunctionInterface #-}
module AIFC2WAV
( aifcToWav
) where

import Foreign.C (withCString, CInt(..), CChar(..))
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Ptr (Ptr)

import TempFile

foreign import ccall unsafe "aifc2wav_main" aifc2wav_main
  :: CInt -> Ptr (Ptr CChar) -> IO CInt

-- | Given a (new-style) IMA4-compressed AIFC file, converts it to a WAV file.
aifcToWav :: FilePath -> TempIO FilePath
aifcToWav aifc = do
  wav <- newTempFile "aifcToWav.wav"
  code <- liftIO $
    withCString "aifc2wav" $ \progC ->
    withCString aifc       $ \aifcC ->
    withCString wav        $ \wavC  ->
    withArrayLen [progC, aifcC, wavC] $ \n v ->
      aifc2wav_main (fromIntegral n) v
  if code == 0
    then return wav
    else error $ "aifcToWav: returned " ++ show code
