module Sound.Jammit.Internal.Sox
( Audio(..)
, Time(..)
, renderAudio
, optimize
) where

import Control.Arrow (first)
import Control.Monad (guard)

import Sound.Jammit.Internal.Audio
import qualified Data.Conduit as C
import qualified Data.Vector as V
import Data.Int

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
  Mix xs -> undefined

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
