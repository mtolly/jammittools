{- |
Basic types and functions for dealing with Jammit song packages.
-}
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
, loadInfo
, loadTracks
, findJammitDir
, songSubdirs
) where

import Control.Applicative ((<$>), (<*>), liftA2)
import Control.Arrow ((***))
import Control.Exception (evaluate)
import Control.Monad (filterM, guard)
import Data.Char (toLower, toUpper)
import Data.Maybe (catMaybes)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import qualified Data.Map as Map
import qualified Data.PropertyList as PL
import qualified System.Directory as Dir
import System.FilePath ((</>))
import qualified System.Info as Info

-- | The Enum instance corresponds to the number used in the "instrument"
-- property, and the names (used by Show/Read) are capitalized versions of those
-- used in the "skillLevel" property.
data Instrument = Guitar | Bass | Drums | Keyboard | Vocal
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Part
  = PartGuitar1 -- ^ Used for both Guitar and Guitar 1
  | PartGuitar2
  | PartBass
  | PartDrums
  | PartKeys1 -- ^ Used for both Keys and Keys 1
  | PartKeys2
  | PartPiano -- ^ Rarely used. Seen in \"The Answer Lies Within\" and \"Wait for Sleep\"
  | PartSynth -- ^ Rarely used. Seen in \"Wait for Sleep\"
  | PartVocal
  | PartBVocals
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data AudioPart
  = Only Part -- ^ An audio file for a single notated part.
  | Without Instrument -- ^ The backing track for an instrument package.
  deriving (Eq, Ord, Show, Read)

data SheetPart
  = Notation Part -- ^ For any instrument, the notation sheet music.
  | Tab Part -- ^ For guitar and bass, the tablature sheet music.
  deriving (Eq, Ord, Show, Read)

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

titleToAudioPart :: String -> Instrument -> Maybe AudioPart
titleToAudioPart "Band" i = Just $ Without i
titleToAudioPart s      _ = Only <$> titleToPart s

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

audioPartToInstrument :: AudioPart -> Instrument
audioPartToInstrument (Only    p) = partToInstrument p
audioPartToInstrument (Without i) = i

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

instance PL.PropertyListItem Info where

  fromPropertyList pl = PL.fromPlDict pl >>= \dict -> Info
    <$> (Map.lookup "album"        dict >>= PL.fromPlString)
    <*> (Map.lookup "artist"       dict >>= PL.fromPlString)
    <*> (Map.lookup "bpm"          dict >>= PL.fromPlString)
    <*> (Map.lookup "copyright"    dict >>= PL.fromPlString)
    <*> (Map.lookup "countInBeats" dict >>= PL.fromPlInt   )
    <*> (Map.lookup "courtesyOf"   dict >>= PL.fromPlString)
    <*> (Map.lookup "demo"         dict >>=    fromPlEnum  )
    <*> (Map.lookup "explicit"     dict >>=    fromPlEnum  )
    <*> (Map.lookup "genre"        dict >>= PL.fromPlString)
    <*> (Map.lookup "instrument"   dict >>=    fromPlEnum  )
    <*> (Map.lookup "publishedBy"  dict >>= PL.fromPlString)
    <*> (Map.lookup "skillLevel"   dict >>=    fromPlSkills)
    <*> (Map.lookup "sku"          dict >>= PL.fromPlString)
    <*> (Map.lookup "slow"         dict >>= PL.fromPlReal  )
    <*> (Map.lookup "title"        dict >>= PL.fromPlString)
    <*> (Map.lookup "version"      dict >>= PL.fromPlInt   )
    <*> (Map.lookup "writtenBy"    dict >>= PL.fromPlString)

  toPropertyList info = PL.plDict $ Map.fromList
    [ ("album"       , PL.plString $ album        info)
    , ("artist"      , PL.plString $ artist       info)
    , ("bpm"         , PL.plString $ bpm          info)
    , ("copyright"   , PL.plString $ copyright    info)
    , ("countInBeats", PL.plInt    $ countInBeats info)
    , ("courtesyOf"  , PL.plString $ courtesyOf   info)
    , ("demo"        ,    plEnum   $ demo         info)
    , ("explicit"    ,    plEnum   $ explicit     info)
    , ("genre"       , PL.plString $ genre        info)
    , ("instrument"  ,    plEnum   $ instrument   info)
    , ("publishedBy" , PL.plString $ publishedBy  info)
    , ("skillLevel"  ,    plSkills $ skillLevel   info)
    , ("sku"         , PL.plString $ sku          info)
    , ("slow"        , PL.plReal   $ slow         info)
    , ("title"       , PL.plString $ title        info)
    , ("version"     , PL.plInt    $ version      info)
    , ("writtenBy"   , PL.plString $ writtenBy    info)
    ]

fromPlEnum :: (Enum a) => PL.PropertyList -> Maybe a
fromPlEnum pl = toEnum . fromIntegral <$> PL.fromPlInt pl

plEnum :: (Enum a) => a -> PL.PropertyList
plEnum = PL.plInt . fromIntegral . fromEnum

fromPlSkills
  :: PL.PropertyList -> Maybe (Either Integer [(Instrument, Integer)])
fromPlSkills pl = case (PL.fromPlInt pl, PL.fromPlDict pl) of
  (Nothing, Nothing) -> Nothing
  (Just i , _      ) -> Just $ Left i
  (_      , Just d ) -> let
    getSkill (x, y) = liftA2 (,) (readMaybe $ capitalize x) (PL.fromPlInt y)
    capitalize ""     = ""
    capitalize (c:cs) = toUpper c : map toLower cs
    in fmap Right $ mapM getSkill $ Map.toList d

plSkills :: Either Integer [(Instrument, Integer)] -> PL.PropertyList
plSkills (Left  i ) = PL.plInt i
plSkills (Right sl) = PL.plDict $
  Map.fromList $ map (map toLower . show *** PL.plInt) sl

loadInfo :: FilePath -> IO (Maybe Info)
loadInfo dir =
  PL.fromPropertyList <$> readXmlPropertyListFromFile' (dir </> "info.plist")

data Track = Track
  { trackClass          :: String
  , identifier          :: String
  , scoreSystemHeight   :: Maybe Integer
  , scoreSystemInterval :: Maybe Integer
  , trackTitle          :: Maybe String
  } deriving (Eq, Ord, Show, Read)

instance PL.PropertyListItem Track where

  toPropertyList t = PL.plDict $ Map.fromList $ catMaybes
    [ Just ("class"              , PL.plString $ trackClass          t)
    , Just ("identifier"         , PL.plString $ identifier          t)
    , (\i -> ("scoreSystemHeight"  , PL.plInt    i)) <$> scoreSystemHeight   t
    , (\i -> ("scoreSystemInterval", PL.plInt    i)) <$> scoreSystemInterval t
    , (\s -> ("title"              , PL.plString s)) <$> trackTitle          t
    ]

  fromPropertyList pl = PL.fromPlDict pl >>= \d -> Track
    <$>      (Map.lookup "class"               d >>= PL.fromPlString)
    <*>      (Map.lookup "identifier"          d >>= PL.fromPlString)
    <*> Just (Map.lookup "scoreSystemHeight"   d >>= PL.fromPlInt   )
    <*> Just (Map.lookup "scoreSystemInterval" d >>= PL.fromPlInt   )
    <*> Just (Map.lookup "title"               d >>= PL.fromPlString)

loadTracks :: FilePath -> IO (Maybe [Track])
loadTracks dir = PL.listFromPropertyList <$>
  readXmlPropertyListFromFile' (dir </> "tracks.plist")

-- | Reads strictly so as not to exhaust our allowed open files.
readXmlPropertyListFromFile' :: FilePath -> IO PL.PropertyList
readXmlPropertyListFromFile' f = do
  str <- readFile f
  _ <- evaluate $ length str
  either fail return $ PL.readXmlPropertyList str

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
