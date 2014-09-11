module Sound.Jammit.Export
( Library
, fuzzySearchBy
, exactSearchBy
, loadLibrary
, getAudioParts
, getSheetParts
, runAudio
, runSheet
) where

import Control.Applicative ((<$>), liftA2)
import Control.Monad (forM)
import Data.Char (toLower)
import Data.List (isInfixOf, transpose, sort, isPrefixOf)
import Data.Maybe (catMaybes)

import System.Directory (getDirectoryContents)
import System.FilePath ((</>), splitFileName, takeFileName)

import Sound.Jammit.Internal.AIFC2WAV
import Sound.Jammit.Internal.ImageMagick
import Sound.Jammit.Base
import Sound.Jammit.Internal.Sox
import Sound.Jammit.Internal.TempFile

type Library = [(FilePath, Info, [Track])]

fuzzySearchBy :: (Info -> String) -> String -> Library -> Library
fuzzySearchBy f str = let str' = map toLower str in
  filter $ \(_, info, _) -> str' `isInfixOf` map toLower (f info)

exactSearchBy :: (Info -> String) -> String -> Library -> Library
exactSearchBy f str = filter $ \(_, info, _) -> f info == str

loadLibrary :: FilePath -> IO Library
loadLibrary jmt = do
  dirs <- songSubdirs jmt
  fmap catMaybes $ forM dirs $ \d -> do
    maybeInfo <- loadInfo   d
    maybeTrks <- loadTracks d
    return $ liftA2 (\i t -> (d, i, t)) maybeInfo maybeTrks

-- | A mapping from audio part to absolute filename of an audio file.
getAudioParts :: Library -> [(AudioPart, FilePath)]
getAudioParts lib = do
  (dir, info, trks) <- lib
  trk <- trks
  case trackTitle trk >>= \t -> titleToAudioPart t (instrument info) of
    Nothing -> []
    Just ap -> [(ap, dir </> (identifier trk ++ "_jcfx"))]

-- | A mapping from sheet part to
-- @(prefix of image files, line height in pixels)@.
getSheetParts :: Library -> [(SheetPart, (FilePath, Integer))]
getSheetParts lib = do
  (dir, _info, trks) <- lib
  trk <- trks
  case (trackTitle trk >>= \t -> titleToPart t, scoreSystemInterval trk) of
    (Just p, Just ht) -> let
      sheet = (Notation p, (dir </> (identifier trk ++ "_jcfn"), ht))
      tab   = (Tab      p, (dir </> (identifier trk ++ "_jcft"), ht))
      in if elem (partToInstrument p) [Guitar, Bass]
        then [sheet, tab]
        else [sheet]
    _ -> []

-- | Takes a list of positive AIFCs, a list of negative AIFCs, and a WAV file
-- to produce.
runAudio :: [FilePath] -> [FilePath] -> FilePath -> IO ()
runAudio pos neg fout = runTempIO fout $ do
  -- I've only found one audio file where the instruments are not aligned:
  -- the drums and drums backing track for Take the Time are 38 samples ahead
  -- of the other instruments. So as a hack, we pad the front of them by 38
  -- samples to line things up.
  let tttDrums     = "793EAAE0-6761-44D7-9A9A-1FB451A2A438_jcfx"
      tttDrumsBack = "37EE5AA5-4049-4CED-844A-D34F6B165F67_jcfx"
      aifcToWav' a = if takeFileName a `elem` [tttDrums, tttDrumsBack]
        then Pad (Samples 38) . File <$> aifcToWav a
        else File <$> aifcToWav a
  posWavs <- map (\w -> ( 1, w)) <$> mapM aifcToWav' pos
  negWavs <- map (\w -> (-1, w)) <$> mapM aifcToWav' neg
  renderAudio $ optimize $ Mix (posWavs ++ negWavs)

runSheet :: [(FilePath, Integer)] -> Int -> FilePath -> IO ()
runSheet trks lns fout = runTempIO fout $ do
  trkLns <- forM trks $ \(fp, ht) -> do
    let (dir, file) = splitFileName fp
    ls <- liftIO $ getDirectoryContents dir
    cnct <- connectVertical $
      map (dir </>) $ sort $ filter (file `isPrefixOf`) ls
    splitVertical ht cnct
  pages <- forM (map concat $ chunksOf lns $ transpose trkLns) $ \pg ->
    connectVertical pg
  joinPages pages

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = case splitAt n xs of
  (ys, zs) -> ys : chunksOf n zs
