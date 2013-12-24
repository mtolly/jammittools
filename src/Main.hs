{-# LANGUAGE TupleSections #-}
module Main (main) where

import qualified System.Directory as Dir
import System.FilePath ((</>), takeFileName)
import Control.Applicative ((<$>), liftA2)
import Control.Monad (forM, forM_, (>=>), guard)
import Data.Char (toLower, isDigit)
import Data.Maybe (mapMaybe, catMaybes, fromMaybe, fromJust)
import Data.List
  (sortBy, stripPrefix, isInfixOf, isPrefixOf, transpose, sort, nub)
import Data.Ord (comparing)
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)

import Jammit
import ImageMagick
import AIFC2WAV
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

partGetTrack :: (Part, SheetType) -> [Track] -> Maybe (String, Integer)
partGetTrack (p, st) trks = let
  suffix = case st of
    Notation -> "_jcfn"
    Tab      -> "_jcft"
  match trk = maybe False (== p) $ titleToPart =<< trackTitle trk
  in case filter match trks of
    []      -> Nothing
    trk : _ -> scoreSystemInterval trk >>= \ssi ->
      Just (identifier trk ++ suffix, ssi)

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

findInstrument :: Instrument -> Library -> Maybe (FilePath, Info, [Track])
findInstrument inst lib =
  case filter (\(_, info, _) -> instrument info == inst) lib of
    [song] -> Just song
    _      -> Nothing

argOpts :: [OptDescr (Args -> Args)]
argOpts =
  [ Option ['t'] ["title"] (ReqArg (\s a -> a { searchTitle = s }) "str")
    "search by song title"
  , Option ['r'] ["artist"] (ReqArg (\s a -> a { searchArtist = s }) "str")
    "search by song artist"
  , Option ['y'] ["yesparts"]
    (ReqArg (\s a -> a { selectParts = s }) "parts")
    "parts to appear in sheet music or audio"
  , Option ['n'] ["noparts"]
    (ReqArg (\s a -> a { rejectParts = s }) "parts")
    "parts to subtract (add inverted) from audio"
  , Option ['l'] ["lines"]
    (ReqArg (\s a -> a { pageLines = Just $ read s }) "int")
    "number of systems per page"
  , Option ['j'] ["jammit"]
    (ReqArg (\s a -> a { jammitDir = Just s }) "directory")
    "location of Jammit library"
  , Option ['?'] ["help"]
    (NoArg $ \a -> a { function = PrintUsage })
    "function: print usage info"
  , Option ['d'] ["database"]
    (NoArg $ \a -> a { function = ShowDatabase })
    "function: display all songs in db"
  , Option ['s'] ["sheet"]
    (ReqArg (\s a -> a { function = ExportSheet s }) "file")
    "function: export sheet music"
  , Option ['a'] ["audio"]
    (ReqArg (\s a -> a { function = ExportAudio s }) "file")
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
  (fs, _, _) <- getOpt Permute argOpts <$> getArgs
  let args = foldr ($) defaultArgs fs
  case function args of
    PrintUsage -> do
      prog <- getProgName
      let header = "Usage: " ++ prog ++ " [options]"
      putStr $ usageInfo header argOpts
    ShowDatabase -> do
      matches <- searchResults args
      putStr $ showLibrary matches
    ExportAudio fout -> do
      matches <- searchResults args
      let insttrks = mapMaybe (\inst -> (inst,) <$> findInstrument inst matches)
            [minBound .. maxBound]
            :: [(Instrument, (FilePath, Info, [Track]))]
          yes = mapMaybe charToAudioPart $ selectParts args
            :: [AudioPart]
          no = mapMaybe charToAudioPart $ rejectParts args
            :: [AudioPart]
      undefined
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
run trks lns fout = do
  pwd <- Dir.getCurrentDirectory
  tmp <- (</> "jammitsheet") <$> Dir.getTemporaryDirectory
  Dir.createDirectoryIfMissing True tmp
  Dir.setCurrentDirectory tmp
  forM_ trks $ \(fp, ht) -> do
    let justFile = takeFileName fp
    connectVertical [fp ++ "*"] (justFile ++ ".png")
    splitVertical ht (justFile ++ ".png") (justFile ++ "_line.png")
  ls <- Dir.getDirectoryContents "."
  let trkLns = flip map trks $ \trk -> sortBy (comparing getNumber) $
        filter (takeFileName (fst trk ++ "_line") `isPrefixOf`) ls
      pages = map concat $ chunksOf lns $ transpose trkLns
  forM_ (zip [0..] pages) $ \(i, fps) ->
    connectVertical fps $ "page_" ++ show4 i ++ ".png"
  let pageNums = zipWith (\i _ -> "page_" ++ show4 i ++ ".png") [0..] pages
  joinPages pageNums $ pwd </> fout

getNumber :: String -> Int
getNumber = read . reverse . takeWhile isDigit .
  fromJust . stripPrefix "gnp." . reverse

show4 :: Int -> String
show4 i = let s = show i in replicate (4 - length s) '0' ++ s

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = case splitAt n xs of
  (ys, zs) -> ys : chunksOf n zs
