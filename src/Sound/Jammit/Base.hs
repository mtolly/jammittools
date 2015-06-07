{- |
Basic types and functions for dealing with Jammit song packages.
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
module Sound.Jammit.Base
( Instrument(..)
, Part(..)
, AudioPart(..)
, SheetPart(..)
, titleToPart
, titleToAudioPart
, partToInstrument
, audioPartToInstrument
, Info(..)
, Track(..)
, SkillLevel(..)
, loadInfo
, loadTracks
, findJammitDir
, songSubdirs
) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Applicative ((<|>))
import Control.Monad (filterM, guard, forM)
import Data.Char (toLower)
import System.Environment (lookupEnv)
import qualified Data.Map as Map
import qualified System.Directory as Dir
import System.FilePath ((</>))
import qualified System.Info as Info

import Sound.Jammit.Internal.PropertyList

-- | The 'Enum' instance corresponds to the number used in the 'instrument'
-- property, and the names (used by 'Show' and 'Read') are capitalized versions
-- of those used in the 'skillLevel' property.
data Instrument = Guitar | Bass | Drums | Keyboard | Vocal
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Used for both the "instrument" and "skillLevel" properties.
instance PropertyListItem Instrument where
  fromPropertyList pl = plistToEnum pl <|> do
    str <- fromPropertyList pl
    lookup str [ (map toLower $ show inst, inst) | inst <- [minBound .. maxBound] ]

data Part
  = PartGuitar1 -- ^ Used for both Guitar and Guitar 1
  | PartGuitar2
  | PartBass
  | PartDrums1 -- ^ Used for both Drums and Drums 1
  | PartDrums2 -- ^ Rarely used. Seen in \"Space Truckin\'\"
  | PartKeys1 -- ^ Used for both Keys and Keys 1
  | PartKeys2
  | PartPiano -- ^ Rarely used. Seen in \"The Answer Lies Within\" and \"Wait for Sleep\"
  | PartSynth -- ^ Rarely used. Seen in \"Wait for Sleep\"
  | PartOrgan -- ^ Rarely used. Seen in \"Smoke on the Water\"
  | PartVocal
  | PartBVocals
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data AudioPart
  = Only    Part       -- ^ An audio file for a single notated part.
  | Without Instrument -- ^ The backing track for an instrument package.
  deriving (Eq, Ord, Show, Read)

data SheetPart
  = Notation Part -- ^ For any instrument, the notation sheet music.
  | Tab      Part -- ^ For guitar and bass, the tablature sheet music.
  deriving (Eq, Ord, Show, Read)

titleToPart :: String -> Maybe Part
titleToPart s = case s of
  "Guitar"   -> Just PartGuitar1
  "Guitar 1" -> Just PartGuitar1
  "Guitar 2" -> Just PartGuitar2
  "Bass"     -> Just PartBass
  "Drums"    -> Just PartDrums1
  "Drums 1"  -> Just PartDrums1
  "Drums 2"  -> Just PartDrums2
  "Keys"     -> Just PartKeys1
  "Keys 1"   -> Just PartKeys1
  "Keys 2"   -> Just PartKeys2
  "Piano"    -> Just PartPiano
  "Synth"    -> Just PartSynth
  "Organ"    -> Just PartOrgan
  "Vocal"    -> Just PartVocal
  "B Vocals" -> Just PartBVocals
  _          -> Nothing

titleToAudioPart :: String -> Instrument -> Maybe AudioPart
titleToAudioPart "Band" i = Just $ Without i
titleToAudioPart s      _ = Only <$> titleToPart s

partToInstrument :: Part -> Instrument
partToInstrument p = case p of
  PartGuitar1 -> Guitar
  PartGuitar2 -> Guitar
  PartBass    -> Bass
  PartDrums1  -> Drums
  PartDrums2  -> Drums
  PartKeys1   -> Keyboard
  PartKeys2   -> Keyboard
  PartPiano   -> Keyboard
  PartSynth   -> Keyboard
  PartOrgan   -> Keyboard
  PartVocal   -> Vocal
  PartBVocals -> Vocal

audioPartToInstrument :: AudioPart -> Instrument
audioPartToInstrument (Only    p) = partToInstrument p
audioPartToInstrument (Without i) = i

data SkillLevel
  = OneSkill   Integer
  | ManySkills [(Instrument, Integer)]
  deriving (Eq, Ord, Show, Read)

-- | Can be an integer, or a dict of instrument names to integers.
instance PropertyListItem SkillLevel where
  fromPropertyList pl
    =   do
      OneSkill <$> fromPropertyList pl
    <|> do
      dict <- fromPropertyList pl
      fmap ManySkills $ forM (Map.toList dict) $ \(k, v) -> do
        inst <- fromPropertyList $ String k
        return (inst, v)

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
  , skillLevel   :: SkillLevel
  , sku          :: String
  , slow         :: Double
  , title        :: String
  , version      :: Integer
  , writtenBy    :: String
  } deriving (Eq, Ord, Show, Read)

instance PropertyListItem Info where
  fromPropertyList pl = do
    dict <- fromPropertyList pl
    album        <- fromLookup "album"        dict
    artist       <- fromLookup "artist"       dict
    bpm          <- fromLookup "bpm"          dict
    copyright    <- fromLookup "copyright"    dict
    countInBeats <- fromLookup "countInBeats" dict
    courtesyOf   <- fromLookup "courtesyOf"   dict
    demo         <- fromLookup "demo"         dict
    explicit     <- fromLookup "explicit"     dict
    genre        <- fromLookup "genre"        dict
    instrument   <- fromLookup "instrument"   dict
    publishedBy  <- fromLookup "publishedBy"  dict
    skillLevel   <- fromLookup "skillLevel"   dict
    sku          <- fromLookup "sku"          dict
    slow         <- fromLookup "slow"         dict
    title        <- fromLookup "title"        dict
    version      <- fromLookup "version"      dict
    writtenBy    <- fromLookup "writtenBy"    dict
    return Info{..}

loadInfo :: FilePath -> IO (Maybe Info)
loadInfo dir = fromPropertyList <$> readPropertyList (dir </> "info.plist")

data Track = Track
  { trackClass          :: String
  , identifier          :: String
  , scoreSystemHeight   :: Maybe Integer
  , scoreSystemInterval :: Maybe Integer
  , trackTitle          :: Maybe String
  } deriving (Eq, Ord, Show, Read)

instance PropertyListItem Track where
  fromPropertyList pl = do
    dict <- fromPropertyList pl
    trackClass             <- fromLookup "class"               dict
    identifier             <- fromLookup "identifier"          dict
    let scoreSystemHeight   = fromLookup "scoreSystemHeight"   dict
        scoreSystemInterval = fromLookup "scoreSystemInterval" dict
        trackTitle          = fromLookup "title"               dict
    return Track{..}

loadTracks :: FilePath -> IO (Maybe [Track])
loadTracks dir = fromPropertyList <$> readPropertyList (dir </> "tracks.plist")

-- | Tries to find the top-level Jammit library directory on Windows or
-- Mac OS X.
findJammitDir :: IO (Maybe FilePath)
findJammitDir = case Info.os of
  "mingw32" -> do
    var <- lookupEnv "LocalAppData"
    case var of
      Just local -> jammitIn local
      Nothing    -> return Nothing
  "darwin" -> do
    home <- Dir.getHomeDirectory
    jammitIn $ home </> "Library" </> "Application Support"
  _ -> return Nothing
  where jammitIn dir = do
          let jmt = dir </> "Jammit"
          b <- Dir.doesDirectoryExist jmt
          return $ guard b >> Just jmt

-- | Gets the contents of a directory without the @.@ and @..@ special paths,
-- and adds the directory to the front of all the names to make absolute paths.
lsAbsolute :: FilePath -> IO [FilePath]
lsAbsolute d =
  map (d </>) . filter (`notElem` [".", ".."]) <$> Dir.getDirectoryContents d

-- | Searches a directory and all subdirectories for folders containing a Jammit
-- info file.
songSubdirs :: FilePath -> IO [FilePath]
songSubdirs dir = do
  isSong <- Dir.doesFileExist $ dir </> "info.plist"
  let here = [dir | isSong]
  subdirs <- lsAbsolute dir >>= filterM Dir.doesDirectoryExist
  (here ++) . concat <$> mapM songSubdirs subdirs
