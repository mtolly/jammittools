{-# LANGUAGE ForeignFunctionInterface #-}
module Sound.Jammit.Internal.AIFC2WAV
( aifcToWav
) where

import Foreign.C (withCWString, CInt(..), CWchar(..))
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Ptr (Ptr)

import Sound.Jammit.Internal.TempFile

foreign import ccall unsafe "aifc2wav_main" aifc2wav_main
  :: CInt -> Ptr (Ptr CWchar) -> IO CInt

-- | Given a (new-style) IMA4-compressed AIFC file, converts it to a WAV file.
aifcToWav :: FilePath -> TempIO FilePath
aifcToWav aifc = do
  wav <- newTempFile "aifcToWav.wav"
  code <- liftIO $
    withCWString "aifc2wav" $ \progC ->
    withCWString aifc       $ \aifcC ->
    withCWString wav        $ \wavC  ->
    withArrayLen [progC, aifcC, wavC] $ \n v ->
      aifc2wav_main (fromIntegral n) v
  if code == 0
    then return wav
    else error $ "aifcToWav: returned " ++ show code
