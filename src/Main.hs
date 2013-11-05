{-# LANGUAGE TupleSections #-}
module Main (main) where

import System.Directory
import System.Process (readProcess, readProcessWithExitCode)
import System.FilePath ((</>))
import System.Exit (ExitCode(..), exitSuccess)
import Control.Applicative ((<$>), liftA2)
import Control.Monad (forM, forM_, void, when)
import Data.Char
import Data.Maybe
import Data.List (sortBy, stripPrefix, isInfixOf, isPrefixOf, transpose)
import Data.Ord (comparing)
import System.Console.GetOpt
import System.Environment (getArgs, lookupEnv, getProgName)
import qualified System.Info as Info
import Jammit

data Args = Args
  { searchTitle  :: String
  , searchArtist :: String
  , sheetParts   :: [Part]
  , pageLines    :: Maybe Int
  , jammitDir    :: Maybe String
  , printUsage   :: Bool
  } deriving (Eq, Ord, Show, Read)

defaultArgs :: Args
defaultArgs = Args
  { searchTitle  = ""
  , searchArtist = ""
  , sheetParts   = []
  , pageLines    = Nothing
  , jammitDir    = Nothing
  , printUsage   = False
  }

data Part
  = NGuitar1
  | TGuitar1
  | NGuitar2
  | TGuitar2
  | NBass
  | TBass
  | NDrums
  | NKeys1
  | NKeys2
  | NPiano
  | NSynth
  | NVocal
  | NBVocals
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

partToInstrument :: Part -> Instrument
partToInstrument p = case p of
  NGuitar1 -> Guitar
  TGuitar1 -> Guitar
  NGuitar2 -> Guitar
  TGuitar2 -> Guitar
  NBass    -> Bass
  TBass    -> Bass
  NDrums   -> Drums
  NKeys1   -> Keyboard
  NKeys2   -> Keyboard
  NPiano   -> Keyboard
  NSynth   -> Keyboard
  NVocal   -> Vocal
  NBVocals -> Vocal

partGetTrack :: Part -> [Track] -> Maybe (String, Integer)
partGetTrack p trks = let
  suffix = if elem p [TGuitar1, TGuitar2, TBass] then "_jcft" else "_jcfn"
  titles = case p of
    NGuitar1 -> ["Guitar", "Guitar 1"]
    TGuitar1 -> ["Guitar", "Guitar 1"]
    NGuitar2 -> ["Guitar 2"]
    TGuitar2 -> ["Guitar 2"]
    NBass    -> ["Bass"]
    TBass    -> ["Bass"]
    NDrums   -> ["Drums"]
    NKeys1   -> ["Keys", "Keys 1"]
    NKeys2   -> ["Keys 2"]
    NPiano   -> ["Piano"]
    NSynth   -> ["Synth"]
    NVocal   -> ["Vocal"]
    NBVocals -> ["B Vocals"]
  in case filter (maybe False (`elem` titles) . trackTitle) trks of
    []      -> Nothing
    trk : _ -> scoreSystemInterval trk >>= \ssi ->
      Just (identifier trk ++ suffix, ssi)

charToPart :: Char -> Maybe Part
charToPart c = lookup c
  [ ('g', NGuitar1)
  , ('u', TGuitar1)
  , ('G', NGuitar2)
  , ('U', TGuitar2)
  , ('b', NBass   )
  , ('a', TBass   )
  , ('d', NDrums  )
  , ('k', NKeys1  )
  , ('K', NKeys2  )
  , ('p', NPiano  )
  , ('s', NSynth  )
  , ('v', NVocal  )
  , ('V', NBVocals)
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
    (ReqArg (\s a -> a { sheetParts = mapMaybe charToPart s }) "guGUbadkKpsvV")
    "parts to appear in music"
  , Option ['l'] ["lines"]
    (ReqArg (\s a -> a { pageLines = Just $ read s }) "int")
    "number of systems per page"
  , Option ['j'] ["jammit"]
    (ReqArg (\s a -> a { jammitDir = Just s }) "DIR")
    "location of Jammit library"
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
      insttrks = mapMaybe (\inst -> (inst,) <$> findInstrument inst matches)
        [minBound..maxBound]
      maybeParts = map
        (\p -> (, p) <$> lookup (partToInstrument p) insttrks)
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
  pwd <- getCurrentDirectory
  tmp <- (</> "jammitsheet") <$> getTemporaryDirectory
  createDirectoryIfMissing True tmp
  setCurrentDirectory tmp
  forM_ fptrks $ \(fp, trk) -> do
    let ident = fst trk
    connectVertical [fp </> (ident ++ "*")] (ident ++ ".png")
    splitVertical (snd trk) (ident ++ ".png") (ident ++ "_line.png")
  ls <- getDirectoryContents "."
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

-- | Find an ImageMagick binary, because the names are way too generic, and
-- "convert" is both an ImageMagick program and a Windows built-in utility.
imageMagick :: String -> IO (Maybe String)
imageMagick cmd = do
  (code, _, _) <- readProcessWithExitCode cmd ["-version"] ""
  case code of
    ExitSuccess -> return $ Just cmd
    _ -> case Info.os of
      "mingw32" -> firstJustM $
        flip map ["ProgramFiles", "ProgramFiles(x86)", "ProgramW6432"] $ \env ->
          lookupEnv env >>= \var -> case var of
            Nothing -> return Nothing
            Just pf
              ->  fmap (\im -> pf </> im </> cmd)
              .   listToMaybe
              .   filter ("ImageMagick" `isPrefixOf`)
              <$> getDirectoryContents pf
      _ -> return Nothing

-- | Only runs actions until the first that gives 'Just'.
firstJustM :: (Monad m) => [m (Maybe a)] -> m (Maybe a)
firstJustM [] = return Nothing
firstJustM (mx : xs) = mx >>= \x -> case x of
  Nothing -> firstJustM xs
  Just y  -> return $ Just y

-- | Uses ImageMagick to stick images together vertically.
connectVertical :: [FilePath] -> FilePath -> IO ()
connectVertical fins fout = do
  cmd <- fromMaybe "montage" <$> imageMagick "montage"
  void $ readProcess cmd
    (["-geometry", "100%", "-tile", "1x"] ++ fins ++ [fout]) ""

-- | Uses ImageMagick to split an image into chunks of a given height.
splitVertical :: Integer -> FilePath -> FilePath -> IO ()
splitVertical i fin fout = do
  cmd <- fromMaybe "convert" <$> imageMagick "convert"
  void $ readProcess cmd ["-crop", "x" ++ show i, fin, fout] ""

-- | Uses ImageMagick to join several images into pages of a PDF.
joinPages :: [FilePath] -> FilePath -> IO ()
joinPages fins fout = do
  cmd <- fromMaybe "convert" <$> imageMagick "convert"
  void $ readProcess cmd (fins ++ [fout]) ""

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = case splitAt n xs of
  (ys, zs) -> ys : chunksOf n zs
