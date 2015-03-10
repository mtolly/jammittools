{- |
Functions for exporting Jammit audio (as WAV) and sheet music (as PDF).
-}
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

import Control.Applicative (liftA2)
import Control.Monad (forM)
import Data.Char (toLower)
import Data.List (isInfixOf, sort, isPrefixOf)
import Data.Maybe (catMaybes)

import System.Directory (getDirectoryContents)
import System.FilePath ((</>), splitFileName, takeFileName)

import Sound.Jammit.Internal.Image
import Sound.Jammit.Base
import Sound.Jammit.Internal.Audio
import Sound.Jammit.Internal.TempIO

import qualified Data.Conduit.Audio as A
import Control.Monad.Trans.Resource (MonadResource, runResourceT)

type Library = [(FilePath, Info, [Track])]

-- | Filter the library based on some string selector. The selector is
-- applied case-insensitively, and the song's field only has to contain the
-- search term rather than match it exactly.
fuzzySearchBy :: (Info -> String) -> String -> Library -> Library
fuzzySearchBy f str = let str' = map toLower str in
  filter $ \(_, info, _) -> str' `isInfixOf` map toLower (f info)

-- | Filter the library based on some string selector. The selector must match
-- exactly.
exactSearchBy :: (Info -> String) -> String -> Library -> Library
exactSearchBy f str = filter $ \(_, info, _) -> f info == str

-- | Given the top-level Jammit library directory, finds all song packages.
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

audioSource :: (MonadResource m) => FilePath -> A.AudioSource m
audioSource fp = if takeFileName fp `elem` [tttDrums, tttDrumsBack]
  then A.padStartFrames 38 $ readIMA fp
  else readIMA fp
  -- I've only found one audio file where the instruments are not aligned:
  -- the drums and drums backing track for Take the Time are 38 samples ahead
  -- of the other instruments. So as a hack, we pad the front of them by 38
  -- samples to line things up.
  where tttDrums     = "793EAAE0-6761-44D7-9A9A-1FB451A2A438_jcfx"
        tttDrumsBack = "37EE5AA5-4049-4CED-844A-D34F6B165F67_jcfx"

runAudio
  :: [FilePath] -- ^ AIFCs to mix in normally
  -> [FilePath] -- ^ AIFCs to mix in inverted
  -> FilePath   -- ^ the resulting WAV file
  -> IO ()
runAudio pos neg fp = let
  src = case (map audioSource pos, map audioSource neg) of
    ([]    , []    ) -> A.silent 0 44100 2
    (p : ps, []    ) -> foldr A.mix p ps
    ([]    , n : ns) -> A.gain (-1) $ foldr A.mix n ns
    (p : ps, n : ns) -> A.mix (foldr A.mix p ps) $ A.gain (-1) $ foldr A.mix n ns
  in runResourceT $ writeWAV fp src

runSheet
  :: [(FilePath, Integer)] -- ^ pairs of @(png file prefix, line height in px)@
  -> Int                   -- ^ how many sheet music systems per page
  -> FilePath              -- ^ the resulting PDF
  -> IO ()
runSheet trks lns fout = runTempIO fout $ do
  trkLns <- liftIO $ forM trks $ \(fp, ht) -> do
    let (dir, file) = splitFileName fp
    ls <- getDirectoryContents dir
    return (map (dir </>) $ sort $ filter (file `isPrefixOf`) ls, ht)
  jpegs <- partsToPages trkLns lns
  pdf <- newTempFile "pages.pdf"
  liftIO $ jpegsToPDF jpegs pdf
  return pdf
