{-# LANGUAGE ViewPatterns #-}
module Jammit
( Instrument(..)
, Part(..)
, SheetType(..)
, titleToPart
, partToInstrument
, Info(..)
, Track(..)
, loadInfo
, loadTracks
, findJammitDir
, songSubdirs
) where

import Control.Applicative ((<$>), (<*>), liftA2)
import Control.Arrow ((***))
import Control.Monad (filterM, guard)

import Data.Char (toLower, toUpper)
import Text.Read (readMaybe)
import Data.Maybe (catMaybes)
import qualified Data.Map as Map

import System.FilePath ((</>))
import System.Directory
  (getHomeDirectory, getDirectoryContents, doesFileExist, doesDirectoryExist)
import qualified System.Info as Info

import Data.PropertyList

-- | The Enum instance corresponds to the number used in the "instrument"
-- property, and the names (used by Show/Read) are capitalized versions of those
-- used in the "skillLevel" property.
data Instrument = Guitar | Bass | Drums | Keyboard | Vocal
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Part
  = PartGuitar1
  | PartGuitar2
  | PartBass
  | PartDrums
  | PartKeys1
  | PartKeys2
  | PartPiano
  | PartSynth
  | PartVocal
  | PartBVocals
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data SheetType = Notation | Tab
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

titleToPart :: String -> Maybe Part
titleToPart s = case s of
  "Guitar"   -> Just PartGuitar1
  "Guitar 1" -> Just PartGuitar1
  "Guitar 2" -> Just PartGuitar2
  "Bass"     -> Just PartBass
  "Drums"    -> Just PartDrums
  "Keys"     -> Just PartKeys1
  "Keys 1"   -> Just PartKeys1
  "Keys 2"   -> Just PartKeys2
  "Piano"    -> Just PartPiano
  "Synth"    -> Just PartSynth
  "Vocal"    -> Just PartVocal
  "B Vocals" -> Just PartBVocals
  _          -> Nothing

partToInstrument :: Part -> Instrument
partToInstrument p = case p of
  PartGuitar1 -> Guitar
  PartGuitar2 -> Guitar
  PartBass    -> Bass
  PartDrums   -> Drums
  PartKeys1   -> Keyboard
  PartKeys2   -> Keyboard
  PartPiano   -> Keyboard
  PartSynth   -> Keyboard
  PartVocal   -> Vocal
  PartBVocals -> Vocal

data Info = Info
  { album        :: String
  , artist       :: String
  , bpm          :: String
  , copyright    :: String
  , countInBeats :: Integer
  , courtesyOf   :: String
  , demo         :: Bool
  , explicit     :: Bool
  , genre        :: String
  , instrument   :: Instrument
  , publishedBy  :: String
  , skillLevel   :: Either Integer [(Instrument, Integer)]
  , sku          :: String
  , slow         :: Double
  , title        :: String
  , version      :: Integer
  , writtenBy    :: String
  } deriving (Eq, Ord, Show, Read)

instance PropertyListItem Info where

  fromPropertyList pl = fromPlDict pl >>= \dict -> Info
    <$> (Map.lookup "album"        dict >>= fromPlString)
    <*> (Map.lookup "artist"       dict >>= fromPlString)
    <*> (Map.lookup "bpm"          dict >>= fromPlString)
    <*> (Map.lookup "copyright"    dict >>= fromPlString)
    <*> (Map.lookup "countInBeats" dict >>= fromPlInt   )
    <*> (Map.lookup "courtesyOf"   dict >>= fromPlString)
    <*> (Map.lookup "demo"         dict >>= fromPlEnum  )
    <*> (Map.lookup "explicit"     dict >>= fromPlEnum  )
    <*> (Map.lookup "genre"        dict >>= fromPlString)
    <*> (Map.lookup "instrument"   dict >>= fromPlEnum  )
    <*> (Map.lookup "publishedBy"  dict >>= fromPlString)
    <*> (Map.lookup "skillLevel"   dict >>= fromPlSkills)
    <*> (Map.lookup "sku"          dict >>= fromPlString)
    <*> (Map.lookup "slow"         dict >>= fromPlReal  )
    <*> (Map.lookup "title"        dict >>= fromPlString)
    <*> (Map.lookup "version"      dict >>= fromPlInt   )
    <*> (Map.lookup "writtenBy"    dict >>= fromPlString)

  toPropertyList info = plDict $ Map.fromList
    [ ("album"       , plString $ album        info)
    , ("artist"      , plString $ artist       info)
    , ("bpm"         , plString $ bpm          info)
    , ("copyright"   , plString $ copyright    info)
    , ("countInBeats", plInt    $ countInBeats info)
    , ("courtesyOf"  , plString $ courtesyOf   info)
    , ("demo"        , plEnum   $ demo         info)
    , ("explicit"    , plEnum   $ explicit     info)
    , ("genre"       , plString $ genre        info)
    , ("instrument"  , plEnum   $ instrument   info)
    , ("publishedBy" , plString $ publishedBy  info)
    , ("skillLevel"  , plSkills $ skillLevel   info)
    , ("sku"         , plString $ sku          info)
    , ("slow"        , plReal   $ slow         info)
    , ("title"       , plString $ title        info)
    , ("version"     , plInt    $ version      info)
    , ("writtenBy"   , plString $ writtenBy    info)
    ]

fromPlEnum :: (Enum a) => PropertyList -> Maybe a
fromPlEnum pl = toEnum . fromIntegral <$> fromPlInt pl

plEnum :: (Enum a) => a -> PropertyList
plEnum = plInt . fromIntegral . fromEnum

fromPlSkills :: PropertyList -> Maybe (Either Integer [(Instrument, Integer)])
fromPlSkills (fromPlDict -> Just d) = let
  getSkill (x, y) = liftA2 (,) (readMaybe $ capitalize x) (fromPlInt y)
  capitalize ""     = ""
  capitalize (c:cs) = toUpper c : map toLower cs
  in fmap Right $ mapM getSkill $ Map.toList d
fromPlSkills (fromPlInt -> Just i) = Just $ Left i
fromPlSkills _ = Nothing

plSkills :: Either Integer [(Instrument, Integer)] -> PropertyList
plSkills (Left  i ) = plInt i
plSkills (Right sl) = plDict $
  Map.fromList $ map (map toLower . show *** plInt) sl

loadInfo :: FilePath -> IO (Maybe Info)
loadInfo dir =
  fromPropertyList <$> readXmlPropertyListFromFile (dir </> "info.plist")

data Track = Track
  { trackClass          :: String
  , identifier          :: String
  , scoreSystemHeight   :: Maybe Integer
  , scoreSystemInterval :: Maybe Integer
  , trackTitle          :: Maybe String
  } deriving (Eq, Ord, Show, Read)

instance PropertyListItem Track where

  toPropertyList t = plDict $ Map.fromList $ catMaybes
    [ Just ("class"              , plString $ trackClass          t)
    , Just ("identifier"         , plString $ identifier          t)
    , scoreSystemHeight   t >>= \i -> Just ("scoreSystemHeight"  , plInt    i)
    , scoreSystemInterval t >>= \i -> Just ("scoreSystemInterval", plInt    i)
    , trackTitle          t >>= \s -> Just ("title"              , plString s)
    ]

  fromPropertyList pl = fromPlDict pl >>= \d -> Track
    <$>      (Map.lookup "class"               d >>= fromPlString)
    <*>      (Map.lookup "identifier"          d >>= fromPlString)
    <*> Just (Map.lookup "scoreSystemHeight"   d >>= fromPlInt   )
    <*> Just (Map.lookup "scoreSystemInterval" d >>= fromPlInt   )
    <*> Just (Map.lookup "title"               d >>= fromPlString)

loadTracks :: FilePath -> IO (Maybe [Track])
loadTracks dir =
  listFromPropertyList <$> readXmlPropertyListFromFile (dir </> "tracks.plist")

-- | Tries to find the top-level Jammit library directory.
findJammitDir :: IO (Maybe FilePath)
findJammitDir = case Info.os of
  "mingw32" -> do
    home <- getHomeDirectory -- C:\Users\foo
    let jmt = home </> "AppData" </> "Local" </> "Jammit"
    b <- doesDirectoryExist jmt
    return $ guard b >> Just jmt
  _ -> return Nothing -- TODO: OS X

-- | Gets the contents of a directory without the @.@ and @..@ special paths,
-- and adds the directory to the front of all the names to make absolute paths.
lsAbsolute :: FilePath -> IO [FilePath]
lsAbsolute dir =
  map (dir </>) . filter (`notElem` [".", ".."]) <$> getDirectoryContents dir

-- | Searches a directory and all subdirectories for folders containing a Jammit
-- info file.
songSubdirs :: FilePath -> IO [FilePath]
songSubdirs dir = do
  isSong <- doesFileExist $ dir </> "info.plist"
  let here = [dir | isSong]
  subdirs <- lsAbsolute dir >>= filterM doesDirectoryExist
  (here ++) . concat <$> mapM songSubdirs subdirs
