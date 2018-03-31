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
, audioSource
, runAudio
, runSheet
, metronomeTrack
, writeMetronomeTrack
) where

import           Control.Applicative          (liftA2)
import           Control.Monad                (forM, forever)
import           Control.Monad.Trans.Resource (MonadResource, runResourceT)
import           Data.Char                    (toLower)
import qualified Data.Conduit.Audio           as A
import           Data.Int                     (Int16, Int32)
import           Data.List                    (isInfixOf, isPrefixOf, sort)
import           Data.Maybe                   (catMaybes, fromMaybe)
import           Sound.Jammit.Base
import           Sound.Jammit.Internal.Audio
import           Sound.Jammit.Internal.Image
import           Sound.Jammit.Internal.TempIO
import           System.Directory             (getDirectoryContents)
import           System.FilePath              (splitFileName, takeFileName,
                                               (</>))

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
      sheet = (Notation p, (dir </> (identifier trk ++ "_jcfn"), ht'))
      tab   = (Tab      p, (dir </> (identifier trk ++ "_jcft"), ht'))
      ht' = case ht of
        0 -> case identifier trk of
          "20C25A80-BFF2-43C6-959A-E284349542CE" -> 129 -- B Vocals for Walking In Memphis
          _ -> 129 -- dunno lol, but assume something so that it's not 0! (eats all memory)
        _ -> ht
      in if elem (partToInstrument p) [Guitar, Bass]
        then [sheet, tab]
        else [sheet]
    _ -> []

audioSource :: (MonadResource m) => FilePath -> IO (A.AudioSource m Int16)
audioSource fp = let
  -- These are hacks that make one instrument line up with the rest of a song.
  timingHacks =
    -- Take the Time (Dream Theater)
    [ ("793EAAE0-6761-44D7-9A9A-1FB451A2A438_jcfx", A.padStart $ A.Frames 38) -- Drums
    , ("37EE5AA5-4049-4CED-844A-D34F6B165F67_jcfx", A.padStart $ A.Frames 38) -- Drums backing
    -- Rockstar (Nickelback)
    , ("FB9A99DB-C70C-47CB-B63F-A78F11AC05D5_jcfx", A.dropStart $ A.Frames 10) -- Vocal
    , ("7998F9B7-8A97-49D0-A3A9-C001FDD1C1DC_jcfx", A.dropStart $ A.Frames 10) -- B Vocals
    , ("C4C543A0-EB1F-4628-A401-C5860D675FA4_jcfx", A.dropStart $ A.Frames 10) -- Vocal backing
    ]
  in fromMaybe id (lookup (takeFileName fp) timingHacks) <$> readIMA fp

runAudio
  :: [FilePath] -- ^ AIFCs to mix in normally
  -> [FilePath] -- ^ AIFCs to mix in inverted
  -> FilePath   -- ^ the resulting WAV file
  -> IO ()
runAudio pos neg fp = do
  pos' <- mapM audioSource pos
  neg' <- mapM audioSource neg
  let src = case (pos', neg') of
        ([]    , []    ) -> A.silent (A.Frames 0) 44100 2
        ([p]   , []    ) -> p
        ([]    , [n]   ) -> A.mapSamples negate16 n
        (p : ps, []    ) -> i32To16 $ mix16To32 p ps
        ([]    , n : ns) -> i32To16 $ A.mapSamples negate $ mix16To32 n ns
        (p : ps, n : ns) -> i32To16 $ A.mix (mix16To32 p ps) $ A.mapSamples negate $ mix16To32 n ns
      i16To32 = A.mapSamples (fromIntegral :: Int16 -> Int32)
      i32To16 = A.mapSamples (fromIntegral . clamp (-32768, 32767) :: Int32 -> Int16)
      negate16 :: Int16 -> Int16
      negate16 x = if x == minBound then maxBound else negate x
      mix16To32 x xs = foldr A.mix (i16To32 x) (map i16To32 xs)
  runResourceT $ writeWAV fp src

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

writeMetronomeTrack :: FilePath -> [Beat] -> IO ()
writeMetronomeTrack fp beats = runResourceT $ writeWAV fp $ metronomeTrack beats

metronomeTrack :: (Monad m) => [Beat] -> A.AudioSource m Int16
metronomeTrack beats = let
  samples = map (\b -> floor $ position b * 44100) beats
  clicks = zipWith makeClick samples $ map Just (drop 1 samples) ++ repeat Nothing
  makeClick f1 (Just f2) = A.takeStart (A.Frames $ f2 - f1) $ A.concatenate metronomeClick infiniteSilence
  makeClick _  Nothing   = metronomeClick
  silentBlock = A.silent (A.Frames A.chunkSize) 44100 2
  zeroAudio = A.silent (A.Frames 0) 44100 2
  infiniteSilence = A.AudioSource
    { A.rate = 44100
    , A.channels = 2
    , A.frames = 0
    , A.source = forever $ A.source silentBlock
    }
  in foldr A.concatenate zeroAudio clicks
