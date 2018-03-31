{- |
Basic types and functions for dealing with Jammit song packages.
-}
{-# LANGUAGE CPP             #-}
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
, Info(..), loadInfo
, Track(..), loadTracks
, SkillLevel(..)
, findJammitDir
, songSubdirs
, Beat(..), loadBeats, loadGhost
, Section(..), loadSections
, findNotation, findTab, findAudio
, sheetWidth, sheetHeight
) where

import           Control.Applicative                ((<|>))
import           Control.Monad                      (filterM, forM, guard)
import           Data.Char                          (toLower)
import qualified Data.Map                           as Map
import           Data.Maybe                         (fromMaybe)
import           Sound.Jammit.Internal.PropertyList
import qualified System.Directory                   as Dir
import           System.Environment                 (lookupEnv)
import           System.FilePath                    (dropTrailingPathSeparator,
                                                     takeFileName, (</>))
import qualified System.Info                        as Info

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative                ((<$>))
#endif

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
  | PartBass1 -- ^ Used for both Bass and Bass 1
  | PartBass2 -- ^ Rarely used. Seen in \"The Fish (Schindleria Praematurus)\", \"I've Seen All Good People\", \"Perpetual Change\"
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
  "Bass"     -> Just PartBass1
  "Bass 1"   -> Just PartBass1
  "Bass 2"   -> Just PartBass2
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
  "Vocal 1"  -> Just PartVocal -- I've Seen All Good People
  "Vocals"   -> Just PartVocal -- South Side of the Sky
  "B Vocals" -> Just PartBVocals
  "Vocal 2"  -> Just PartBVocals -- I've Seen All Good People
  _          -> Nothing

titleToAudioPart :: String -> Instrument -> Maybe AudioPart
titleToAudioPart "Band" i = Just $ Without i
titleToAudioPart s      _ = Only <$> titleToPart s

partToInstrument :: Part -> Instrument
partToInstrument p = case p of
  PartGuitar1 -> Guitar
  PartGuitar2 -> Guitar
  PartBass1   -> Bass
  PartBass2   -> Bass
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
    explicit     <- fromLookup "explicit"     dict <|> return False
    genre        <- fromLookup "genre"        dict
    instrument   <- fromLookup "instrument"   dict
    publishedBy  <- fromLookup "publishedBy"  dict
    skillLevel   <- fromLookup "skillLevel"   dict <|> return (OneSkill 0)
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
songSubdirs dir = case takeFileName $ dropTrailingPathSeparator dir of
  fn@('.' : _ : _) | fn /= ".." -> return []
  _                             -> do
    isSong <- and <$> mapM Dir.doesFileExist
      [dir </> "info.plist", dir </> "tracks.plist"]
    let here = [dir | isSong]
    subdirs <- lsAbsolute dir >>= filterM Dir.doesDirectoryExist
    (here ++) . concat <$> mapM songSubdirs subdirs

data Beat = Beat
  { isDownbeat  :: Bool
  , isGhostBeat :: Bool
  , position    :: Double
  } deriving (Eq, Ord, Show, Read)

instance PropertyListItem Beat where
  fromPropertyList pl = do
    dict <- fromPropertyList pl
    let isDownbeat  = fromMaybe False $ fromLookup "isDownbeat"  dict
        isGhostBeat = fromMaybe False $ fromLookup "isGhostBeat" dict
    position       <-                   fromLookup "position"    dict
    return Beat{..}

loadBeats :: FilePath -> IO (Maybe [Beat])
loadBeats dir = fromPropertyList <$> readPropertyList (dir </> "beats.plist")

loadGhost :: FilePath -> IO (Maybe [Beat])
loadGhost dir = fromPropertyList <$> readPropertyList (dir </> "ghost.plist")

data Section = Section
  { sectionBeat   :: Integer
  , sectionNumber :: Integer
  , sectionType   :: Integer
  } deriving (Eq, Ord, Show, Read)

instance PropertyListItem Section where
  fromPropertyList pl = do
    dict <- fromPropertyList pl
    sectionBeat   <- fromLookup "beat"   dict
    sectionNumber <- fromLookup "number" dict
    sectionType   <- fromLookup "type"   dict
    return Section{..}

loadSections :: FilePath -> IO (Maybe [Section])
loadSections dir = fromPropertyList <$> readPropertyList (dir </> "sections.plist")

{-
Known section types
=== in Erotomania ===
0 pre-song
1 intro
2 verse
4 chorus
7 outro
9 post-song
13 b-section
17 interlude
23 c-section
25 guitar solo
36 transition
-}

findImages :: String -> Track -> FilePath -> IO [FilePath]
findImages suffix trk dir = do
  files <- Dir.getDirectoryContents dir
  let image i = identifier trk ++ "_" ++ suffix ++ "_" ++ showTwo i
      showTwo i = if i < 10 then '0' : show i else show i
  return
    [ dir </> file
    | file <- map image ([0..99] :: [Int])
    , file `elem` files
    ]

findNotation, findTab :: Track -> FilePath -> IO [FilePath]
findNotation = findImages "jcfn"
findTab      = findImages "jcft"

findAudio :: Track -> FilePath -> IO (Maybe FilePath)
findAudio trk dir = let
  file = dir </> identifier trk ++ "_jcfx"
  in do
    b <- Dir.doesFileExist file
    return $ guard b >> Just file

sheetWidth, sheetHeight :: (Num a) => a
sheetWidth  = 724
sheetHeight = 1024
