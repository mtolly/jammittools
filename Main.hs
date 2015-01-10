module Main (main) where

import Control.Applicative ((<$>), (<|>))
import Control.Monad ((>=>), forM_, unless)
import Data.Char (toLower)
import Data.List (sort, nub, partition)
import Data.Maybe (mapMaybe, fromMaybe, listToMaybe)
import Data.Version (showVersion)
import qualified System.Console.GetOpt as Opt
import qualified System.Environment as Env
import System.Exit (exitFailure)

import System.FilePath ((</>))
import Text.PrettyPrint.Boxes
  (text, vcat, left, render, hsep, top, (/+/))

import Sound.Jammit.Base
import Sound.Jammit.Export
import qualified Paths_jammittools as Paths

printUsage :: IO ()
printUsage = do
  prog <- Env.getProgName
  putStrLn $ "jammittools v" ++ showVersion Paths.version
  let header = "Usage: " ++ prog ++ " [options]"
  putStr $ Opt.usageInfo header argOpts

main :: IO ()
main = do
  (opts, nonopts, errs) <- Opt.getOpt Opt.Permute argOpts <$> Env.getArgs
  unless (null $ nonopts ++ errs) $ do
    forM_ nonopts $ \nonopt -> putStrLn $ "unrecognized argument `" ++ nonopt ++ "'"
    forM_ errs putStr
    printUsage
    exitFailure
  let args = foldr ($) defaultArgs opts
  case function args of
    PrintUsage -> printUsage
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
    CheckPresence -> do
      matches <- getAudioParts <$> searchResults args
      let f = mapM (`getOneResult` matches) . mapMaybe charToAudioPart
      case (f $ selectParts args, f $ rejectParts args) of
        (Left  err   , _           ) -> error err
        (_           , Left  err   ) -> error err
        (Right _     , Right _     ) -> return ()
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

-- | If there is exactly one pair with the given first element, returns its
-- second element. Otherwise (for 0 or >1 elements) returns an error.
getOneResult :: (Eq a, Show a) => a -> [(a, b)] -> Either String b
getOneResult x xys = case [ b | (a, b) <- xys, a == x ] of
  [y] -> Right y
  ys  -> Left $ "Got " ++ show (length ys) ++ " results for " ++ show x

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
      Nothing -> let
        err = "Couldn't find Jammit directory. Try -j or the env var JAMMIT"
        in fromMaybe (error err) <$> findJammitDir
  db <- loadLibrary jmt
  return $ filterLibrary args db

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
  , Opt.Option ['y'] ["yes-parts"]
    (Opt.ReqArg (\s a -> a { selectParts = s }) "parts")
    "parts to appear in sheet music or audio"
  , Opt.Option ['n'] ["no-parts"]
    (Opt.ReqArg (\s a -> a { rejectParts = s }) "parts")
    "parts to subtract (add inverted) from audio"
  , Opt.Option ['l'] ["lines"]
    (Opt.ReqArg (\s a -> a { pageLines = Just $ read s }) "int")
    "number of systems per page"
  , Opt.Option ['j'] ["jammit"]
    (Opt.ReqArg (\s a -> a { jammitDir = Just s }) "directory")
    "location of Jammit library"
  , Opt.Option ['?', 'h'] ["help"]
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
  , Opt.Option ['c'] ["check"]
    (Opt.NoArg $ \a -> a { function = CheckPresence })
    "function: check presence of audio parts"
  ]

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
  | CheckPresence
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
  PartDrums1  -> 'd'
  PartDrums2  -> 'm'
  PartKeys1   -> 'k'
  PartKeys2   -> 'y'
  PartPiano   -> 'p'
  PartSynth   -> 's'
  PartOrgan   -> 'o'
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
