module Sox
( Audio(..)
, renderAudio
) where

import Data.Monoid (Monoid(..))

import System.Process (readProcess)

import TempFile

data Audio
  = Silence
  | File FilePath
  | Invert Audio
  | Mix Audio Audio
  deriving (Eq, Ord, Show, Read)

instance Monoid Audio where
  mappend = Mix
  mempty  = Silence

mixedFiles :: Audio -> ([FilePath], [FilePath])
mixedFiles Silence    = ([], [])
mixedFiles (File   f) = ([f], [])
mixedFiles (Invert x) = let (a, b) = mixedFiles x in (b, a)
mixedFiles (Mix  x y) = let
  (a, b) = mixedFiles x
  (c, d) = mixedFiles y
  in (a ++ c, b ++ d)

renderAudio :: Audio -> TempIO FilePath
renderAudio aud = do
  fout <- newTempFile "render.wav"
  let (norm, inv) = mixedFiles aud
      makeNormal x = ["-v", "1", x]
      makeInvert x = ["-v", "-1", x]
      args = case (norm, inv) of
        ([] , [] ) -> ["-n", fout, "trim", "0", "0"]
        ([x], [] ) -> makeNormal x ++ [fout]
        ([] , [x]) -> makeInvert x ++ [fout]
        (_  , _  ) -> ["--combine", "mix"]
          ++ concatMap makeNormal norm
          ++ concatMap makeInvert inv
          ++ [fout]
  _ <- liftIO $ readProcess "sox" args ""
  return fout
