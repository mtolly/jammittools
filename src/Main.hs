module Main (main) where

import Control.Applicative ((<$>), liftA2, (<|>))
import Control.Monad (forM, (>=>), forM_)
import Data.Char (toLower)
import Data.Either (rights)
import Data.List (isInfixOf, transpose, sort, nub, partition, isPrefixOf)
import Data.Maybe (mapMaybe, catMaybes, fromMaybe, listToMaybe)
import Data.Version (showVersion)
import qualified System.Console.GetOpt as Opt
import qualified System.Environment as Env

import System.Directory (getDirectoryContents)
import System.FilePath ((</>), splitFileName, takeFileName)
import Text.PrettyPrint.Boxes
  (text, vcat, left, render, hsep, top, (/+/))

import AIFC2WAV
import ImageMagick
import Jammit
import qualified Paths_jammittools as Paths
import Sox
import TempFile

data Args = Args
  { filterLibrary :: Library -> Library
  , selectParts   :: String
  , rejectParts   :: String
  , pageLines     :: Maybe Int
  , jammitDir     :: Maybe FilePath
  , function      :: Function
  }

data Function
  = PrintUsage
  | ShowDatabase
  | ExportSheet FilePath
  | ExportAudio FilePath
  | ExportAll   FilePath
  | TryAudio    FilePath
  | TryBacking  FilePath
  deriving (Eq, Ord, Show, Read)

defaultArgs :: Args
defaultArgs = Args
  { filterLibrary = id
  , selectParts   = ""
  , rejectParts   = ""
  , pageLines     = Nothing
  , jammitDir     = Nothing
  , function      = PrintUsage
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

fuzzySearchBy :: (Info -> String) -> String -> Library -> Library
fuzzySearchBy f str = let str' = map toLower str in
  filter $ \(_, info, _) -> str' `isInfixOf` map toLower (f info)

exactSearchBy :: (Info -> String) -> String -> Library -> Library
exactSearchBy f str = filter $ \(_, info, _) -> f info == str

argOpts :: [Opt.OptDescr (Args -> Args)]
argOpts =
  [ Opt.Option ['t'] ["title"]
    (Opt.ReqArg
      (\s a -> a { filterLibrary = fuzzySearchBy title s . filterLibrary a })
      "str")
    "search by song title"
  , Opt.Option ['r'] ["artist"]
    (Opt.ReqArg
      (\s a -> a { filterLibrary = fuzzySearchBy artist s . filterLibrary a })
      "str")
    "search by song artist"
  , Opt.Option ['T'] ["title-exact"]
    (Opt.ReqArg
      (\s a -> a { filterLibrary = exactSearchBy title s . filterLibrary a })
      "str")
    "search by song title (exact)"
  , Opt.Option ['R'] ["artist-exact"]
    (Opt.ReqArg
      (\s a -> a { filterLibrary = exactSearchBy artist s . filterLibrary a })
      "str")
    "search by song artist (exact)"
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
  , Opt.Option ['x'] ["export"]
    (Opt.ReqArg (\s a -> a { function = ExportAll s }) "dir")
    "function: export all to dir"
  , Opt.Option ['u'] ["tryaudio"]
    (Opt.ReqArg (\s a -> a { function = TryAudio s }) "file")
    "function: see below"
  , Opt.Option ['b'] ["trybacking"]
    (Opt.ReqArg (\s a -> a { function = TryBacking s }) "file")
    "function: see below"
  ]

loadLibrary :: FilePath -> IO Library
loadLibrary jmt = do
  dirs <- songSubdirs jmt
  fmap catMaybes $ forM dirs $ \d -> do
    maybeInfo <- loadInfo   d
    maybeTrks <- loadTracks d
    return $ liftA2 (\i t -> (d, i, t)) maybeInfo maybeTrks

-- | Displays a table of the library, possibly filtered by search terms.
showLibrary :: Library -> String
showLibrary lib = let
  titleArtists = sort $ nub [ (title info, artist info) | (_, info, _) <- lib ]
  partsFor ttl art = map partToChar $ sort $ concat
    [ mapMaybe (trackTitle >=> titleToPart) trks
    | (_, info, trks) <- lib
    , (ttl, art) == (title info, artist info) ]
  makeColumn h col = text h /+/ vcat left (map text col)
  titleColumn  = makeColumn "Title"  $ map fst                titleArtists
  artistColumn = makeColumn "Artist" $ map snd                titleArtists
  partsColumn  = makeColumn "Parts"  $ map (uncurry partsFor) titleArtists
  in render $ hsep 1 top [titleColumn, artistColumn, partsColumn]

-- | Loads the Jammit library, and applies the search terms from the arguments
-- to filter it.
searchResults :: Args -> IO Library
searchResults args = do
  jmt <- case jammitDir args of
    Just j  -> return j
    Nothing -> Env.lookupEnv "JAMMIT" >>= \mv -> case mv of
      Just j -> return j
      Nothing ->
        fromMaybe (error "Couldn't find Jammit directory.") <$> findJammitDir
  db <- loadLibrary jmt
  return $ filterLibrary args db

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

-- | If there is exactly one pair with the given first element, returns its
-- second element. Otherwise (for 0 or >1 elements) returns an error.
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
      putStrLn $ "jammittools v" ++ showVersion Paths.version
      let header = "Usage: " ++ prog ++ " [options]"
      putStr $ Opt.usageInfo header argOpts
      mapM_ putStrLn
        [ "-y <parts> -u <file> exports an audio file where parts not in the"
        , "library are replaced with silence."
        , "-n <instruments> -b <file> exports an audio file that tries to"
        , "remove the given instruments if those backing tracks are present."
        , "Combining the two functions, each part will show up in *either* the"
        , "tryaudio or trybacking files depending on whether you have it."
        ]
    ShowDatabase -> do
      matches <- searchResults args
      putStr $ showLibrary matches
    ExportAudio fout -> do
      matches <- getAudioParts <$> searchResults args
      let f = mapM (`getOneResult` matches) . mapMaybe charToAudioPart
      case (f $ selectParts args, f $ rejectParts args) of
        (Left  err   , _           ) -> error err
        (_           , Left  err   ) -> error err
        (Right yaifcs, Right naifcs) -> runAudio yaifcs naifcs fout
    TryAudio fout -> do
      matches <- getAudioParts <$> searchResults args
      let selected = rights $ map (`getOneResult` matches) $
            mapMaybe charToAudioPart $ selectParts args
      runAudio selected [] fout
    TryBacking fout -> do
      matches <- getAudioParts <$> searchResults args
      let rejected = map audioPartToInstrument $ mapMaybe charToAudioPart $
            rejectParts args
          backingOrder = [Drums, Guitar, Keyboard, Bass, Vocal]
          possibleBacks =
            [ (i, f)
            | i <- backingOrder
            , i `elem` rejected
            , (Without i', f) <- matches
            , i == i'
            ]
      case possibleBacks of
        [] -> case [ (i, f) | (Without i, f) <- matches ] of
          []         -> error "No Jammit backing track found."
          (i, f) : _ -> let
            addFiles = [ f' | (Only p, f') <- matches, partToInstrument p == i ]
            in runAudio (f : addFiles) [] fout
        (i, f) : _ -> let
          remInsts = filter (/= i) rejected
          remFiles =
            [ f'
            | (Only p, f') <- matches
            , partToInstrument p `elem` remInsts
            ]
          in runAudio [f] remFiles fout
    ExportSheet fout -> do
      matches <- getSheetParts <$> searchResults args
      let f = mapM (`getOneResult` matches) . mapMaybe charToSheetPart
      case f $ selectParts args of
        Left  err   -> error err
        Right parts -> let
          systemHeight = sum $ map snd parts
          in runSheet parts (getPageLines systemHeight args) fout
    ExportAll dout -> do
      matches <- searchResults args
      let sheets = getSheetParts matches
          audios = getAudioParts matches
          backingOrder = [Drums, Guitar, Keyboard, Bass, Vocal]
          isGuitar p = elem (partToInstrument p) [Guitar, Bass]
          (gtrs, nongtrs) = partition isGuitar [minBound .. maxBound]
          chosenBacking = listToMaybe $ flip mapMaybe backingOrder $ \i ->
            case getOneResult (Without i) audios of
              Left  _  -> Nothing
              Right fp -> Just (i, fp)
      forM_ gtrs $ \p ->
        case (getOneResult (Notation p) sheets, getOneResult (Tab p) sheets) of
          (Right note, Right tab) -> let
            parts = [note, tab]
            systemHeight = sum $ map snd parts
            fout = dout </> drop 4 (map toLower (show p) ++ ".pdf")
            in runSheet [note, tab] (getPageLines systemHeight args) fout
          _ -> return ()
      forM_ nongtrs $ \p ->
        case getOneResult (Notation p) sheets of
          Left  _    -> return ()
          Right note -> let
            fout = dout </> drop 4 (map toLower (show p) ++ ".pdf")
            in runSheet [note] (getPageLines (snd note) args) fout
      forM_ [minBound .. maxBound] $ \p ->
        case getOneResult (Only p) audios of
          Left  _  -> return ()
          Right fp -> let
            fout = dout </> drop 4 (map toLower (show p) ++ ".wav")
            in runAudio [fp] [] fout
      case chosenBacking of
        Nothing            -> return ()
        Just (inst, fback) -> let
          others = [ fp | (Only p, fp) <- audios, partToInstrument p /= inst ]
          fout = dout </> "backing.wav"
          in runAudio [fback] others fout

getPageLines :: Integer -> Args -> Int
getPageLines systemHeight args = let
  pageHeight   = 724 / 8.5 * 11 :: Double
  defaultLines = round $ pageHeight / fromIntegral systemHeight
  in max 1 $ fromMaybe defaultLines $ pageLines args

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
