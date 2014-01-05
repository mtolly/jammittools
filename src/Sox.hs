module Sox
( Audio(..)
, Time(..)
, renderAudio
, optimize
) where

import Control.Arrow (first)
import Control.Monad (void, forM, guard)

import System.Process (readProcess)

import TempFile

data Audio
  = Empty                 -- ^ An empty stereo file
  | File FilePath         -- ^ An existing (stereo) file
  | Pad Time Audio        -- ^ Pad audio start with silence
  | Mix [(Double, Audio)] -- ^ Add audio sample-wise, also multiplying volumes
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
        readProcess "sox" ["-v", show d, fin, fout] ""
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

optimize :: Audio -> Audio
optimize aud = case aud of
  Pad (Samples 0) x -> x
  Pad (Seconds 0) x -> x
  Mix xs -> let
    xs' = do
      (d, x) <- xs
      guard $ d /= 0
      case optimize x of
        Mix ys -> map (first (* d)) ys
        x'     -> [(d, x')]
    in case xs' of
      []       -> Empty
      [(1, x)] -> x
      _        -> Mix xs'
  Concat xs -> let
    xs' = do
      x <- xs
      case optimize x of
        Concat ys -> ys
        x'        -> [x']
    in case xs' of
      []  -> Empty
      [x] -> x
      _   -> Concat xs'
  _ -> aud
