{-# LANGUAGE LambdaCase #-}
module Sound.Jammit.Internal.AudioExpr
( Audio(..)
, Time(..)
, renderAudio
, optimize
) where

import Control.Arrow (first)
import Control.Monad (guard, forever)

import Sound.Jammit.Internal.Audio
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Vector as V
import Data.Int
import Data.Maybe (isNothing, catMaybes)
import Control.Arrow ((***))

data Audio
  = Empty                 -- ^ An empty stereo file
  | File FilePath         -- ^ A Jammit-provided AIFC file
  | Pad Time Audio        -- ^ Pad audio start with silence
  | Mix [(Double, Audio)] -- ^ Add audio sample-wise, also multiplying volumes
  | Concat [Audio]        -- ^ Sequentially connect audio
  deriving (Eq, Ord, Show, Read)

data Time
  = Seconds Double
  | Samples Integer
  deriving (Eq, Ord, Show, Read)

renderAudio :: Audio -> FilePath -> IO ()
renderAudio aud wavout = renderSource aud C.$$ writeWAV wavout

renderSource :: Audio -> C.Source IO (V.Vector (Int16, Int16))
renderSource aud = case aud of
  Empty -> return ()
  File f -> readIMA f
  Pad t x -> do
    let samples = case t of
          Samples s -> fromIntegral s
          Seconds s -> floor $ s * 44100
    C.yield $ V.replicate samples (0, 0)
    renderSource x
  Concat xs -> mapM_ renderSource xs
  Mix xs -> let
    toSampleSource :: (Double, Audio) -> C.Source IO (Double, Double)
    toSampleSource (p, x) = renderSource x C.=$= CL.concatMap
      (V.toList . V.map (\(l, r) -> (multiplyBy p l, multiplyBy p r)))
    multiplyBy :: Double -> Int16 -> Double
    multiplyBy p i16 = (fromIntegral i16 / 32767) * p
    zipped :: C.Source IO [Maybe (Double, Double)]
    zipped = C.sequenceSources
      [ (toSampleSource px C.=$= CL.map Just) >> forever (C.yield Nothing) | px <- xs ]
    loop = C.await >>= \case
      Nothing -> return () -- shouldn't happen
      Just maybeSamples -> if all isNothing maybeSamples
        then return ()
        else let
          mixed = doubleToInt16 *** doubleToInt16 $ foldr mix (0, 0) $ catMaybes maybeSamples
          mix (l1, r1) (l2, r2) = (l1 + l2, r1 + r2)
          in C.yield (V.singleton mixed) >> loop
    doubleToInt16 :: Double -> Int16
    doubleToInt16 d =
      if d > 1 then maxBound else if d < (-1) then minBound else round $ d * 32767
    in zipped C.=$= loop

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
