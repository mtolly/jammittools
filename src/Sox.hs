module Sox
( Audio(..)
, Time(..)
, renderAudio
) where

import Control.Monad (void, forM)

import System.Process (readProcess)

import TempFile

data Audio
  = Empty                 -- ^ An empty stereo file
  | File FilePath         -- ^ An existing (stereo) file
  | Pad Time Audio        -- ^ Pad audio start with silence
  | Mix [(Double, Audio)] -- ^ Add audio sample-wise, also changing volumes
  | Concat [Audio]        -- ^ Sequentially connect audio
  deriving (Eq, Ord, Show, Read)

data Time
  = Seconds Double
  | Samples Integer
  deriving (Eq, Ord, Show, Read)

showTime :: Time -> String
showTime (Seconds d) = show d
showTime (Samples i) = show i ++ "s"

renderAudio :: Audio -> TempIO FilePath
renderAudio aud = case aud of
  Empty -> do
    fout <- newTempFile "render.wav"
    void $ liftIO $
      readProcess "sox" (["-n", fout] ++ words "trim 0 0 channels 2") ""
    return fout
  File f -> return f
  Pad t x -> do
    fin <- renderAudio x
    fout <- newTempFile "render.wav"
    void $ liftIO $ readProcess "sox" [fin, fout, "pad", showTime t] ""
    return fout
  Mix xs -> case xs of
    [] -> renderAudio Empty
    [(d, x)] -> do
      fin <- renderAudio x
      fout <- newTempFile "render.wav"
      void $ liftIO $
        readProcess "sox" ["--combine", "mix", "-v", show d, fin, fout] ""
      return fout
    _ -> do
      dfins <- forM xs $ \(d, x) -> do
        fin <- renderAudio x
        return (d, fin)
      let argsin = concatMap
            (\(d, fin) -> ["--combine", "mix", "-v", show d, fin]) dfins
      fout <- newTempFile "render.wav"
      void $ liftIO $ readProcess "sox" (argsin ++ [fout]) ""
      return fout
  Concat xs -> case xs of
    [] -> renderAudio Empty
    _ -> do
      fins <- mapM renderAudio xs
      fout <- newTempFile "render.wav"
      void $ liftIO $ readProcess "sox" (fins ++ [fout]) ""
      return fout
