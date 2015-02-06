{-# LANGUAGE LambdaCase #-}
module Sound.Jammit.Internal.AudioExpr
( Audio(..)
, Time(..)
, renderAudio
, optimize
) where

import Control.Arrow (first, (***))
import Control.Monad (guard, forever)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Data.Conduit.Internal (zipSources)
import qualified Data.Vector as V
import Data.Int (Int16)
import Data.Maybe (fromMaybe)

import Sound.Jammit.Internal.Audio

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
    toDoubles :: (Double, Audio) -> C.Source IO (V.Vector (Double, Double))
    toDoubles (p, x) = renderSource x C.=$= CL.map (V.map $ multiplyBy p *** multiplyBy p)
    multiplyBy :: Double -> Int16 -> Double
    multiplyBy p i16 = (fromIntegral i16 / 32767) * p
    doubleToInt16 :: Double -> Int16
    doubleToInt16 d =
      if d > 1 then maxBound else if d < (-1) then minBound else round $ d * 32767
    in foldr mixAudio (return ()) (map toDoubles xs) C.=$= CL.map (V.map $ doubleToInt16 *** doubleToInt16)

mixAudio
  :: C.Source IO (V.Vector (Double, Double))
  -> C.Source IO (V.Vector (Double, Double))
  -> C.Source IO (V.Vector (Double, Double))
mixAudio s1 s2 = let
  justify src = (src C.=$= CL.map Just) >> forever (C.yield Nothing)
  nothingPanic = error "mixAudio: internal error! reached end of infinite stream"
  mix = V.zipWith $ \(l1, r1) (l2, r2) -> (l1 + l2, r1 + r2)
  in zipSources (justify s1) (justify s2) C.=$= let
    loop = C.await >>= \case
      Nothing -> nothingPanic
      Just pair -> case pair of
        (Nothing, Nothing) -> return ()
        (Just v1, Nothing) -> C.yield v1 >> loop
        (Nothing, Just v2) -> C.yield v2 >> loop
        (Just v1, Just v2) -> case compare (V.length v1) (V.length v2) of
          EQ -> C.yield (mix v1 v2) >> loop
          LT -> let
            (v2a, v2b) = V.splitAt (V.length v1) v2
            in C.yield (mix v1 v2a) >> C.await >>= \case
              Nothing -> nothingPanic
              Just (next1, next2) -> do
                C.leftover (next1, Just $ v2b V.++ fromMaybe V.empty next2)
                loop
          GT -> C.leftover (Just v2, Just v1) >> loop
    in loop

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
