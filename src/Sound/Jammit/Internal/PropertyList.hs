-- | A very simple, non-robust property list parser.
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
module Sound.Jammit.Internal.PropertyList
( readPropertyList
, PropertyList(..)
, PropertyListItem(..)
, plistToEnum
, fromLookup
) where

import qualified Data.Text.IO as TIO
import Text.XML.Light
import qualified Data.Map as Map
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe, fromJust)
import Data.Char (isSpace)
import Control.Monad (guard)

#if !MIN_VERSION_base(4,8,0)
import Prelude hiding (mapM)
import Data.Traversable (mapM)
import Control.Applicative ((<$>))
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

asChild :: Element -> Maybe (String, String)
asChild Element{ elName = QName{..}, .. } = case elContent of
  [Text CData{..}] -> Just (qName, cdData)
  _                -> Nothing

plist :: Element -> Maybe PropertyList
plist e = case asParent e of
  ("plist", [x]) -> value x
  _              -> Nothing

value :: Element -> Maybe PropertyList
value e = case asParent e of
  ("array", elts) -> Array <$> mapM value elts
  ("dict" , elts) -> Dict . Map.fromList <$> go elts where
    go (x : y : xs) = do
      ("key", k) <- asChild x
      v <- value y
      ((k, v) :) <$> go xs
    go [] = Just []
    go _  = Nothing
  ("true" , []) -> Just $ Bool True
  ("false", []) -> Just $ Bool False
  _ -> asChild e >>= \case
    ("string" , s) -> Just $ String $ trim s
    ("real"   , s) -> Real    <$> readMaybe s
    ("integer", s) -> Integer <$> readMaybe s
    _              -> Nothing
    where trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- | Reads strictly so as not to exhaust our allowed open files.
readPropertyList :: FilePath -> IO PropertyList
readPropertyList f = do
  txt <- TIO.readFile f
  case parseXMLDoc txt >>= plist of
    Nothing -> error $
      "readPropertyList: failed to read property list from " ++ f
    Just pl -> return pl

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
