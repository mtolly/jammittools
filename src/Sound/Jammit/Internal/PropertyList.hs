-- | A very simple, non-robust property list parser.
{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecordWildCards   #-}
module Sound.Jammit.Internal.PropertyList
( readPropertyList
, PropertyList(..)
, PropertyListItem(..)
, plistToEnum
, fromLookup
) where

import           Control.Monad       (guard)
import           Data.Char           (isSpace)
import qualified Data.Map            as Map
import           Data.Maybe          (fromJust, mapMaybe)
import qualified Data.Text.IO        as TIO
import           Text.Read           (readMaybe)
import           Text.XML.Light

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative ((<$>))
import           Data.Traversable    (mapM)
import           Prelude             hiding (mapM)
#endif

data PropertyList
  = String String
  | Real Double
  | Integer Integer
  | Bool Bool
  | Array [PropertyList]
  | Dict (Map.Map String PropertyList)
  -- not supported: date or data
  deriving (Eq, Ord, Show, Read)

asParent :: Element -> (String, [Element])
asParent Element{ elName = QName{..}, .. } = (qName, getElements elContent)

getElements :: [Content] -> [Element]
getElements = mapMaybe $ \case
  Elem e -> Just e
  _      -> Nothing

asChild :: Element -> Either String (String, String)
asChild Element{ elName = QName{..}, .. } = case elContent of
  [Text CData{..}] -> Right (qName, cdData)
  []               -> Right (qName, "")
  _                -> Left "(asChild) expected a single text child"

plist :: Element -> Either String PropertyList
plist e = case asParent e of
  ("plist", [x]) -> value x
  _              -> Left "(plist) expected a single <plist> parent tag"

value :: Element -> Either String PropertyList
value e = case asParent e of
  ("array", elts) -> Array <$> mapM value elts
  ("dict" , elts) -> Dict . Map.fromList <$> go elts where
    go (x : y : xs) = asChild x >>= \case
      ("key", k) -> do
        v <- value y
        ((k, v) :) <$> go xs
      _ -> Left "(value) expected <key> at start of <dict>"
    go [] = Right []
    go _  = Left "(value) odd number of children when parsing a dict"
  ("true" , []) -> Right $ Bool True
  ("false", []) -> Right $ Bool False
  _ -> asChild e >>= \case
    ("string" , s) -> Right $ String $ trim s
    ("real"   , s) -> maybe (Left "(value) couldn't parse a <real>") (Right . Real) $ readMaybe s
    ("integer", s) -> maybe (Left "(value) couldn't parse an <integer>") (Right . Integer) $ readMaybe s
    _              -> Left "(value) expected <string> <real> or <integer>"
    where trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- | Reads strictly so as not to exhaust our allowed open files.
readPropertyList :: FilePath -> IO PropertyList
readPropertyList f = do
  txt <- TIO.readFile f
  case parseXMLDoc txt of
    Nothing -> error $ "readPropertyList: couldn't parse XML from " ++ f
    Just xml -> case plist xml of
      Left e -> error $ "readPropertyList: failed to read property list from " ++ f ++ " : " ++ e
      Right pl -> return pl

-- | Only covers parsing values from property lists.
class PropertyListItem a where
  fromPropertyList :: PropertyList -> Maybe a
  listFromPropertyList :: PropertyList -> Maybe [a]
  listFromPropertyList (Array xs) = mapM fromPropertyList xs
  listFromPropertyList _          = Nothing

instance PropertyListItem PropertyList where
  fromPropertyList = Just

instance PropertyListItem Char where
  fromPropertyList (String [c]) = Just c
  fromPropertyList _            = Nothing
  listFromPropertyList (String s) = Just s
  listFromPropertyList _          = Nothing

instance (PropertyListItem a) => PropertyListItem [a] where
  fromPropertyList = listFromPropertyList

instance PropertyListItem Int where
  fromPropertyList (Integer i) = Just $ fromIntegral i
  fromPropertyList _           = Nothing

instance PropertyListItem Integer where
  fromPropertyList (Integer i) = Just i
  fromPropertyList _           = Nothing

instance PropertyListItem Double where
  fromPropertyList (Real d) = Just d
  fromPropertyList _        = Nothing

instance (PropertyListItem a) => PropertyListItem (Map.Map String a) where
  fromPropertyList (Dict d) = mapM fromPropertyList d
  fromPropertyList _        = Nothing

instance PropertyListItem Bool where
  fromPropertyList (Bool    b) = Just b
  fromPropertyList (Integer 0) = Just False
  fromPropertyList (Integer 1) = Just True
  fromPropertyList _           = Nothing

plistToEnum :: (Enum a, Bounded a) => PropertyList -> Maybe a
plistToEnum pl = let
  minval = fromEnum $ minBound `asTypeOf` fromJust result
  maxval = fromEnum $ maxBound `asTypeOf` fromJust result
  result = do
    n <- fromPropertyList pl
    guard $ minval <= n && n <= maxval
    return $ toEnum n
  in result

fromLookup :: (PropertyListItem a) => String -> Map.Map String PropertyList -> Maybe a
fromLookup s dict = Map.lookup s dict >>= fromPropertyList
