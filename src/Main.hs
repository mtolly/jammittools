module Main (main) where

import Control.Applicative ((<$>), liftA2)
import Control.Monad (forM, (>=>), guard)
import Data.Char (toLower)
import Data.List (isInfixOf, transpose, sort, nub)
import Data.Maybe (mapMaybe, catMaybes, fromMaybe)
import Data.Monoid (mconcat)
import qualified System.Console.GetOpt as Opt
import qualified System.Environment as Env

import qualified System.Directory as Dir
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import AIFC2WAV
import ImageMagick
import Jammit
import Sox

data Args = Args
  { searchTitle  :: String
  , searchArtist :: String
  , selectParts  :: String
  , rejectParts  :: String
  , pageLines    :: Maybe Int
  , jammitDir    :: Maybe FilePath
  , function     :: Function
  } deriving (Eq, Ord, Show, Read)

data Function
  = PrintUsage
  | ShowDatabase
  | ExportSheet FilePath
  | ExportAudio FilePath
  deriving (Eq, Ord, Show, Read)

defaultArgs :: Args
defaultArgs = Args
  { searchTitle  = ""
  , searchArtist = ""
  , selectParts  = ""
  , rejectParts  = ""
  , pageLines    = Nothing
  , jammitDir    = Nothing
  , function     = PrintUsage
  }

charToPart :: Char -> Maybe (Part, SheetType)
charToPart c = lookup c
  [ ('g', (PartGuitar1, Notation))
  , ('G', (PartGuitar1, Tab     ))
  , ('r', (PartGuitar2, Notation))
  , ('R', (PartGuitar2, Tab     ))
  , ('b', (PartBass   , Notation))
  , ('B', (PartBass   , Tab     ))
  , ('d', (PartDrums  , Notation))
  , ('k', (PartKeys1  , Notation))
  , ('y', (PartKeys2  , Notation))
  , ('p', (PartPiano  , Notation))
  , ('s', (PartSynth  , Notation))
  , ('v', (PartVocal  , Notation))
  , ('x', (PartBVocals, Notation))
  ]

charToAudioPart :: Char -> Maybe AudioPart
charToAudioPart c = lookup c
  [ ('g', Only PartGuitar1)
  , ('r', Only PartGuitar2)
  , ('b', Only PartBass   )
  , ('d', Only PartDrums  )
  , ('k', Only PartKeys1  )
  , ('y', Only PartKeys2  )
  , ('p', Only PartPiano  )
  , ('s', Only PartSynth  )
  , ('v', Only PartVocal  )
  , ('x', Only PartBVocals)
  , ('G', Without Guitar  )
  , ('B', Without Bass    )
  , ('D', Without Drums   )
  , ('K', Without Keyboard)
  , ('V', Without Vocal   )
  ]

type Library = [(FilePath, Info, [Track])]

searchBy :: (Info -> String) -> String -> Library -> Library
searchBy f str = let str' = map toLower str in
  filter $ \(_, info, _) -> str' `isInfixOf` map toLower (f info)

argOpts :: [Opt.OptDescr (Args -> Args)]
argOpts =
  [ Opt.Option ['t'] ["title"]
    (Opt.ReqArg (\s a -> a { searchTitle = s }) "str")
    "search by song title"
  , Opt.Option ['r'] ["artist"]
    (Opt.ReqArg (\s a -> a { searchArtist = s }) "str")
    "search by song artist"
  , Opt.Option ['y'] ["yesparts"]
    (Opt.ReqArg (\s a -> a { selectParts = s }) "parts")
    "parts to appear in sheet music or audio"
  , Opt.Option ['n'] ["noparts"]
    (Opt.ReqArg (\s a -> a { rejectParts = s }) "parts")
    "parts to subtract (add inverted) from audio"
  , Opt.Option ['l'] ["lines"]
    (Opt.ReqArg (\s a -> a { pageLines = Just $ read s }) "int")
    "number of systems per page"
  , Opt.Option ['j'] ["jammit"]
    (Opt.ReqArg (\s a -> a { jammitDir = Just s }) "directory")
    "location of Jammit library"
  , Opt.Option ['?'] ["help"]
    (Opt.NoArg $ \a -> a { function = PrintUsage })
    "function: print usage info"
  , Opt.Option ['d'] ["database"]
    (Opt.NoArg $ \a -> a { function = ShowDatabase })
    "function: display all songs in db"
  , Opt.Option ['s'] ["sheet"]
    (Opt.ReqArg (\s a -> a { function = ExportSheet s }) "file")
    "function: export sheet music"
  , Opt.Option ['a'] ["audio"]
    (Opt.ReqArg (\s a -> a { function = ExportAudio s }) "file")
    "function: export audio"
  ]

loadLibrary :: FilePath -> IO Library
loadLibrary jmt = do
  dirs <- songSubdirs jmt
  fmap catMaybes $ forM dirs $ \d -> do
    maybeInfo <- loadInfo   d
    maybeTrks <- loadTracks d
    return $ liftA2 (\i t -> (d, i, t)) maybeInfo maybeTrks

showLibrary :: Library -> String
showLibrary lib = let
  titleArtists = sort $ nub [ (title info, artist info) | (_, info, _) <- lib ]
  charForPart p = case p of
    PartGuitar1 -> 'g'
    PartGuitar2 -> 'r'
    PartBass    -> 'b'
    PartDrums   -> 'd'
    PartKeys1   -> 'k'
    PartKeys2   -> 'y'
    PartPiano   -> 'p'
    PartSynth   -> 's'
    PartVocal   -> 'v'
    PartBVocals -> 'x'
  partsFor ttl art = map charForPart $ sort $ concat
    [ mapMaybe (trackTitle >=> titleToPart) trks
    | (_, info, trks) <- lib
    , (ttl, art) == (title info, artist info) ]
  titleArtistParts = [ (t, a, partsFor t a) | (t, a) <- titleArtists ]
  titleWidth  = (+1) $ max 5 $ maximum [ length t | (t, _, _) <- titleArtistParts ]
  artistWidth = (+1) $ max 6 $ maximum [ length a | (_, a, _) <- titleArtistParts ]
  partsWidth  =                maximum [ length p | (_, _, p) <- titleArtistParts ]
  s `padTo` n = take n $ s ++ repeat ' '
  padTAP (t, a, p) = concat
    [ t `padTo` titleWidth
    , a `padTo` artistWidth
    , p ]
  header = padTAP ("Title", "Artist", "Parts")
  divider = replicate (titleWidth + artistWidth + partsWidth) '='
  in unlines $ header : divider : map padTAP titleArtistParts

searchResults :: Args -> IO Library
searchResults args = do
  jmt <- case jammitDir args of
    Nothing ->
      fromMaybe (error "Couldn't find Jammit directory.") <$> findJammitDir
    Just j  -> return j
  db <- loadLibrary jmt
  return $
    searchBy title (searchTitle args) $ searchBy artist (searchArtist args) db

findAudioPart :: AudioPart -> Library -> Either String (FilePath, Track)
findAudioPart ap lib = let
  inst = audioPartToInstrument ap
  filtered = do
    (fp, i, trks) <- lib
    guard $ instrument i == inst
    trk <- trks
    guard $ (trackTitle trk >>= \t -> titleToAudioPart t inst) == Just ap
    return (fp, trk)
  in case filtered of
    [one] -> Right one
    []    -> Left $ "No results for " ++ show ap
    _     -> Left $ "More than one result for " ++ show ap

findSheetPart
  :: (Part, SheetType) -> Library -> Either String (FilePath, Track, SheetType)
findSheetPart (p, st) lib =
  (\(fp, trk) -> (fp, trk, st)) <$> findAudioPart (Only p) lib

getAudioFile :: FilePath -> Track -> FilePath
getAudioFile fp trk = fp </> (identifier trk ++ "_jcfx")

getSheetFile :: FilePath -> Track -> SheetType -> (FilePath, Integer)
getSheetFile fp trk st = let
  suffix = case st of
    Notation -> "_jcfn"
    Tab      -> "_jcft"
  in (fp </> (identifier trk ++ suffix), fromMaybe 0 $ scoreSystemInterval trk)

main :: IO ()
main = do
  (fs, _, _) <- Opt.getOpt Opt.Permute argOpts <$> Env.getArgs
  let args = foldr ($) defaultArgs fs
  case function args of
    PrintUsage -> do
      prog <- Env.getProgName
      let header = "Usage: " ++ prog ++ " [options]"
      putStr $ Opt.usageInfo header argOpts
    ShowDatabase -> do
      matches <- searchResults args
      putStr $ showLibrary matches
    ExportAudio fout -> do
      matches <- searchResults args
      let yes = mapM (`findAudioPart` matches) $ mapMaybe charToAudioPart $ selectParts args
          no = mapM (`findAudioPart` matches) $ mapMaybe charToAudioPart $ rejectParts args
      case (fmap (map $ uncurry getAudioFile) yes, fmap (map $ uncurry getAudioFile) no) of
        (Left  err   , _           ) -> error err
        (_           , Left  err   ) -> error err
        (Right yaifcs, Right naifcs) ->
          withSystemTempDirectory "jammitaudio" $ \tmp -> do
            ywavs <- map File <$> mapM (`aifcToWav` tmp) yaifcs
            nwavs <- map File <$> mapM (`aifcToWav` tmp) naifcs
            wav <- renderAudio (mconcat ywavs `Mix` Invert (mconcat nwavs)) tmp
            Dir.copyFile wav fout
    ExportSheet fout -> do
      matches <- searchResults args
      let sheetParts = mapMaybe charToPart $ selectParts args
            :: [(Part, SheetType)]
      case mapM (`findSheetPart` matches) sheetParts of
        Left  err   -> error err
        Right parts -> let
          uncurry3 f (x, y, z) = f x y z
          realTrks = map (uncurry3 getSheetFile) parts
          systemHeight = sum $ map snd realTrks
          pageHeight = 724 / 8.5 * 11 :: Double
          defaultLines = round $ pageHeight / fromIntegral systemHeight
          in run realTrks (max 1 $ fromMaybe defaultLines $ pageLines args) fout

run :: [(FilePath, Integer)] -> Int -> FilePath -> IO ()
run trks lns fout = withSystemTempDirectory "jammitsheet" $ \tmp -> do
  trkLns <- forM trks $ \(fp, ht) -> do
    cnct <- connectVertical [fp ++ "*"] tmp
    splitVertical ht cnct tmp
  pages <- forM (map concat $ chunksOf lns $ transpose trkLns) $ \pg ->
    connectVertical pg tmp
  pdf <- joinPages pages tmp
  Dir.copyFile pdf fout

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = case splitAt n xs of
  (ys, zs) -> ys : chunksOf n zs
