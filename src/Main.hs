module Main (main) where

import Control.Applicative ((<$>), liftA2, (<|>))
import Control.Monad (forM, (>=>))
import Data.Char (toLower)
import Data.List (isInfixOf, transpose, sort, nub, foldl')
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

partToChar :: Part -> Char
partToChar p = case p of
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

charPartMap :: [(Char, Part)]
charPartMap = [ (partToChar p, p) | p <- [minBound .. maxBound] ]

charToSheetPart :: Char -> Maybe SheetPart
charToSheetPart c = let
  notation = Notation <$> lookup c           charPartMap
  tab      = Tab      <$> lookup (toLower c) charPartMap
  in notation <|> tab

charToAudioPart :: Char -> Maybe AudioPart
charToAudioPart c = let
  only    = Only                       <$> lookup c           charPartMap
  without = Without . partToInstrument <$> lookup (toLower c) charPartMap
  in only <|> without

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
  partsFor ttl art = map partToChar $ sort $ concat
    [ mapMaybe (trackTitle >=> titleToPart) trks
    | (_, info, trks) <- lib
    , (ttl, art) == (title info, artist info) ]
  titleArtistParts = [ (t, a, partsFor t a) | (t, a) <- titleArtists ]
  titleWidth  = foldl' max 5 [ length t | (t, _, _) <- titleArtistParts ]
  artistWidth = foldl' max 6 [ length a | (_, a, _) <- titleArtistParts ]
  partsWidth  = foldl' max 5 [ length p | (_, _, p) <- titleArtistParts ]
  s `padTo` n = take n $ s ++ repeat ' '
  padTAP (t, a, p) = concat
    [ t `padTo` titleWidth
    , " "
    , a `padTo` artistWidth
    , " "
    , p ]
  header = padTAP ("Title", "Artist", "Parts")
  divider = replicate (titleWidth + 1 + artistWidth + 1 + partsWidth) '='
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

getAudioParts :: Library -> [(AudioPart, FilePath)]
getAudioParts lib = do
  (dir, info, trks) <- lib
  trk <- trks
  case trackTitle trk >>= \t -> titleToAudioPart t (instrument info) of
    Nothing -> []
    Just ap -> [(ap, dir </> (identifier trk ++ "_jcfx"))]

getSheetParts :: Library -> [(SheetPart, (FilePath, Integer))]
getSheetParts lib = do
  (dir, _info, trks) <- lib
  trk <- trks
  case (trackTitle trk >>= \t -> titleToPart t, scoreSystemInterval trk) of
    (Just p, Just ht) -> if elem (partToInstrument p) [Guitar, Bass]
      then [sheet, tab]
      else [sheet]
      where sheet = (Notation p, (dir </> (identifier trk ++ "_jcfn"), ht))
            tab   = (Tab      p, (dir </> (identifier trk ++ "_jcft"), ht))
    _ -> []

getOneResult :: (Eq a, Show a) => a -> [(a, b)] -> Either String b
getOneResult x xys = case [ b | (a, b) <- xys, a == x ] of
  [y] -> Right y
  ys  -> Left $ "Got " ++ show (length ys) ++ " results for " ++ show x

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
      matches <- getAudioParts <$> searchResults args
      let f = mapM (`getOneResult` matches)
            . mapMaybe charToAudioPart
      case (f $ selectParts args, f $ rejectParts args) of
        (Left  err   , _           ) -> error err
        (_           , Left  err   ) -> error err
        (Right yaifcs, Right naifcs) ->
          withSystemTempDirectory "jammitaudio" $ \tmp -> do
            ywavs <- map File <$> mapM (`aifcToWav` tmp) yaifcs
            nwavs <- map File <$> mapM (`aifcToWav` tmp) naifcs
            wav <- renderAudio (mconcat ywavs `Mix` Invert (mconcat nwavs)) tmp
            Dir.copyFile wav fout
    ExportSheet fout -> do
      matches <- getSheetParts <$> searchResults args
      let f = mapM (`getOneResult` matches)
            . mapMaybe charToSheetPart
      case f $ selectParts args of
        Left  err   -> error err
        Right parts -> let
          systemHeight = sum $ map snd parts
          pageHeight = 724 / 8.5 * 11 :: Double
          defaultLines = round $ pageHeight / fromIntegral systemHeight
          realLines = max 1 $ fromMaybe defaultLines $ pageLines args
          in runSheet parts realLines fout

runSheet :: [(FilePath, Integer)] -> Int -> FilePath -> IO ()
runSheet trks lns fout = withSystemTempDirectory "jammitsheet" $ \tmp -> do
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
