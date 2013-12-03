{-# LANGUAGE TupleSections #-}
module Main (main) where

import qualified System.Directory as Dir
import System.FilePath ((</>))
import System.Exit (exitSuccess)
import Control.Applicative ((<$>), liftA2)
import Control.Monad (forM, forM_, when, (>=>))
import Data.Char (toLower, isDigit)
import Data.Maybe (mapMaybe, catMaybes, fromMaybe, fromJust)
import Data.List
  (sortBy, stripPrefix, isInfixOf, isPrefixOf, transpose, sort, nub)
import Data.Ord (comparing)
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)

import Jammit
import ImageMagick

data Args = Args
  { searchTitle  :: String
  , searchArtist :: String
  , sheetParts   :: [(Part, SheetType)]
  , pageLines    :: Maybe Int
  , jammitDir    :: Maybe String
  , printUsage   :: Bool
  , showDatabase :: Bool
  } deriving (Eq, Ord, Show, Read)

defaultArgs :: Args
defaultArgs = Args
  { searchTitle  = ""
  , searchArtist = ""
  , sheetParts   = []
  , pageLines    = Nothing
  , jammitDir    = Nothing
  , printUsage   = False
  , showDatabase = False
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
  , Option ['a'] ["artist"] (ReqArg (\s a -> a { searchArtist = s }) "str")
    "search by song artist"
  , Option ['p'] ["parts"]
    (ReqArg (\s a -> a { sheetParts = mapMaybe charToPart s }) "gGrRbBdkypsvx")
    "parts to appear in music"
  , Option ['l'] ["lines"]
    (ReqArg (\s a -> a { pageLines = Just $ read s }) "int")
    "number of systems per page"
  , Option ['j'] ["jammit"]
    (ReqArg (\s a -> a { jammitDir = Just s }) "DIR")
    "location of Jammit library"
  , Option ['d'] ["database"]
    (NoArg $ \a -> a { showDatabase = True })
    "display all songs in db"
  , Option ['?'] ["help"]
    (NoArg $ \a -> a { printUsage = True })
    "print usage info"
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

main :: IO ()
main = do
  (fs, nonopts, _) <- getOpt Permute argOpts <$> getArgs
  let args = foldr ($) defaultArgs fs
      fout = case nonopts of
        f : _ -> f
        []    -> "jammit_out.pdf"
  when (printUsage args) $ do
    prog <- getProgName
    let header = "Usage: " ++ prog ++ " [options] out.pdf"
    putStr $ usageInfo header argOpts
    exitSuccess
  jmt <- case jammitDir args of
    Nothing ->
      fromMaybe (error "Couldn't find Jammit directory.") <$> findJammitDir
    Just j  -> return j
  db <- loadLibrary jmt
  let matches = searchBy title (searchTitle args) $
        searchBy artist (searchArtist args) db
  when (showDatabase args) $ do
    putStr $ showLibrary matches
    exitSuccess
  let insttrks = mapMaybe (\inst -> (inst,) <$> findInstrument inst matches)
        [minBound..maxBound]
      maybeParts = map
        (\(p, st) -> (, (p, st)) <$> lookup (partToInstrument p) insttrks)
        (sheetParts args)
  case sequence maybeParts of
    Nothing    -> putStrLn "Couldn't find a part."
    Just parts -> let
      realTrk ((fp, _, trks), p) = (fp,) <$> partGetTrack p trks
      in case mapM realTrk parts of
        Just real -> let
          systemHeight = sum $ map (snd . snd) real
          pageHeight = 724 / 8.5 * 11 :: Double
          defaultLines = round $ pageHeight / fromIntegral systemHeight
          in run real (fromMaybe defaultLines $ pageLines args) fout
        Nothing   -> putStrLn "Couldn't find a track within a part."

run :: [(FilePath, (String, Integer))] -> Int -> FilePath -> IO ()
run fptrks lns fout = do
  pwd <- Dir.getCurrentDirectory
  tmp <- (</> "jammitsheet") <$> Dir.getTemporaryDirectory
  Dir.createDirectoryIfMissing True tmp
  Dir.setCurrentDirectory tmp
  forM_ fptrks $ \(fp, trk) -> do
    let ident = fst trk
    connectVertical [fp </> (ident ++ "*")] (ident ++ ".png")
    splitVertical (snd trk) (ident ++ ".png") (ident ++ "_line.png")
  ls <- Dir.getDirectoryContents "."
  let trks = map snd fptrks
      trkLns = flip map trks $ \trk -> sortBy (comparing getNumber) $
        filter ((fst trk ++ "_line") `isPrefixOf`) ls
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
