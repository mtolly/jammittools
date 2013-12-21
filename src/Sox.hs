module Sox
( Audio(..)
, render
) where

import Control.Monad (void)
import System.Process (readProcess)
import Data.String (IsString(..))

data Audio
  = Silence
  | File FilePath
  | Invert Audio
  | Mix Audio Audio
  deriving (Eq, Ord, Show, Read)

instance Num Audio where
  (+)           = Mix
  negate        = Invert
  fromInteger 0 = Silence

instance IsString Audio where
  fromString = File

mixedFiles :: Audio -> ([FilePath], [FilePath])
mixedFiles Silence    = ([], [])
mixedFiles (File   f) = ([f], [])
mixedFiles (Invert x) = let (a, b) = mixedFiles x in (b, a)
mixedFiles (Mix  x y) = let
  (a, b) = mixedFiles x
  (c, d) = mixedFiles y
  in (a ++ c, b ++ d)

render :: Audio -> FilePath -> IO ()
render aud fout = let
  (norm, inv) = mixedFiles aud
  makeNormal x = ["-v", "1", x]
  makeInvert x = ["-v", "-1", x]
  args = ["--combine", "mix"]
    ++ concatMap makeNormal norm
    ++ concatMap makeInvert inv
    ++ [fout]
  in void $ readProcess "sox" args ""
