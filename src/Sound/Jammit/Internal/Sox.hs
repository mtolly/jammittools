module Sound.Jammit.Internal.Sox
( Audio(..)
, Time(..)
, renderAudio
, optimize
) where

import Control.Arrow (first)
import Control.Monad (forM_, guard)
import Data.List (transpose)

import Data.WAVE

import Sound.Jammit.Internal.TempFile

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

getHeader :: [WAVE] -> Maybe WAVEHeader
getHeader wavs = let
  eraseLength header = header { waveFrames = Nothing }
  in case map (eraseLength . waveHeader) wavs of
    []     -> Nothing
    h : hs -> do
      forM_ [waveNumChannels, waveFrameRate, waveBitsPerSample]
        $ \f -> guard $ all (== f h) $ map f hs
      return h

renderAudio :: Audio -> TempIO FilePath
renderAudio aud = do
  wav <- liftIO $ renderWAVE aud
  fout <- newTempFile "render.wav"
  liftIO $ putWAVEFile fout wav
  return fout

renderWAVE :: Audio -> IO WAVE
renderWAVE aud = case aud of
  Empty -> return $ WAVE (WAVEHeader 2 44100 16 $ Just 0) []
  File f -> getWAVEFile f
  Pad t x -> do
    wav <- renderWAVE x
    let oldHeader = waveHeader wav
    let samples = case t of
          Seconds s -> floor $ s * fromIntegral (waveFrameRate oldHeader)
          Samples s -> fromIntegral s
        newHeader = oldHeader
          { waveFrames = fmap (+ samples) $ waveFrames oldHeader }
        emptyFrame = replicate (waveNumChannels oldHeader) 0
        newSamples = replicate samples emptyFrame ++ waveSamples wav
    return $ WAVE newHeader newSamples
  Concat [] -> renderWAVE Empty
  Concat xs -> do
    wavs <- mapM renderWAVE xs
    case getHeader wavs of
      Nothing -> error "renderWAVE: concat'd audio files with different formats"
      Just h  -> let
        newHeader = h { waveFrames = fmap sum $ mapM (waveFrames . waveHeader) wavs }
        newSamples = concat $ map waveSamples wavs
        in return $ WAVE newHeader newSamples
  Mix [] -> renderWAVE Empty
  Mix xs -> do
    wavs <- mapM renderWAVE $ map snd xs
    case getHeader wavs of
      Nothing -> error "renderWAVE: mixed audio files with different formats"
      Just h  -> let
        newHeader = h { waveFrames = fmap maximum $ mapM (waveFrames . waveHeader) wavs }
        newSamples = let
          adjusted :: [[[Double]]]
          adjusted = do
            (vol, samps) <- zip (map fst xs) (map waveSamples wavs)
            let adjust i32 = sampleToDouble i32 * vol
            return $ map (map adjust) samps
          mixed :: [[Double]]
          mixed = do
            frameOfFiles <- transpose adjusted
            return $ foldr (zipWith (+)) (repeat 0) frameOfFiles
          in map (map doubleToSample) mixed
        in return $ WAVE newHeader newSamples

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
