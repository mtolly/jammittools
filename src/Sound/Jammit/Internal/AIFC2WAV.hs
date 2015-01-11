{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Sound.Jammit.Internal.AIFC2WAV
( aifcToWav
) where

#ifdef WIDE_CBITS

import Foreign.C (withCWString, CInt(..), CWchar(..))
#define My_CChar CWchar
#define my_withCString withCWString

#else

import Foreign.C (withCString, CInt(..), CChar(..))
#define My_CChar CChar
#define my_withCString withCString

#endif

import Foreign.Ptr (Ptr)

import Sound.Jammit.Internal.TempFile

foreign import ccall unsafe "aifc2wav_main" aifc2wav_main
  :: Ptr My_CChar -> Ptr My_CChar -> IO CInt

-- | Given a (new-style) IMA4-compressed AIFC file, converts it to a WAV file.
aifcToWav :: FilePath -> TempIO FilePath
aifcToWav aifc = do
  wav <- newTempFile "aifcToWav.wav"
  code <- liftIO $
    my_withCString aifc       $ \aifcC ->
    my_withCString wav        $ \wavC  ->
      aifc2wav_main aifcC wavC
  if code == 0
    then return wav
    else error $ "aifcToWav: returned " ++ show code
