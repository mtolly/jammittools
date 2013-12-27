module ImageMagick
( connectVertical
, splitVertical
, joinPages
) where

import System.Directory (getDirectoryContents)
import System.Process (readProcess, readProcessWithExitCode)
import System.FilePath ((</>))
import System.Exit (ExitCode(..))
import Control.Applicative ((<$>))
import Control.Monad (void)
import Data.Maybe (listToMaybe)
import Data.List (isPrefixOf, sortBy)
import Data.Ord (comparing)
import Data.Char (isDigit)
import System.Environment (lookupEnv)
import qualified System.Info as Info
import System.IO.Temp (createTempDirectory)
import TempFile

-- | Find an ImageMagick binary, because the names are way too generic, and
-- "convert" is both an ImageMagick program and a Windows built-in utility.
imageMagick :: String -> IO (Maybe String)
imageMagick cmd = do
  (code, _, _) <- readProcessWithExitCode cmd ["-version"] ""
  case code of
    ExitSuccess -> return $ Just cmd
    _ -> case Info.os of
      "mingw32" -> firstJustM $
        -- env variables for different configs of (ghc arch)/(imagemagick arch)
        -- ProgramFiles: 32/32 or 64/64
        -- ProgramFiles(x86): 64/32
        -- ProgramW6432: 32/64
        flip map ["ProgramFiles", "ProgramFiles(x86)", "ProgramW6432"] $ \env ->
          lookupEnv env >>= \var -> case var of
            Nothing -> return Nothing
            Just pf
              ->  fmap (\im -> pf </> im </> cmd)
              .   listToMaybe
              .   filter ("ImageMagick" `isPrefixOf`)
              <$> getDirectoryContents pf
      _ -> return Nothing

imageMagick' :: String -> IO String
imageMagick' cmd = imageMagick cmd >>= \ms -> case ms of
  Nothing -> error $ "imageMagick': couldn't find program " ++ cmd
  Just s  -> return s

-- | Only runs actions until the first that gives 'Just'.
firstJustM :: (Monad m) => [m (Maybe a)] -> m (Maybe a)
firstJustM [] = return Nothing
firstJustM (mx : xs) = mx >>= \x -> case x of
  Nothing -> firstJustM xs
  Just y  -> return $ Just y

-- | Uses ImageMagick to stick images together vertically.
connectVertical :: [FilePath] -> TempIO FilePath
connectVertical fins = do
  cmd <- liftIO $ imageMagick' "montage"
  fout <- newTempFile "connectVertical.png"
  void $ liftIO $ readProcess cmd
    (["-geometry", "100%", "-tile", "1x"] ++ fins ++ [fout]) ""
  return fout

-- | Uses ImageMagick to split an image into chunks of a given height.
splitVertical :: Integer -> FilePath -> TempIO [FilePath]
splitVertical i fin = do
  cmd <- liftIO $ imageMagick' "convert"
  tempdir <- ask
  splitdir <- liftIO $ createTempDirectory tempdir "splitVertical"
  void $ liftIO $
    readProcess cmd ["-crop", "x" ++ show i, fin, splitdir </> "x.png"] ""
  map (splitdir </>) . sortBy (comparing getNumber) . filter isFile
    <$> liftIO (getDirectoryContents splitdir)
  where getNumber :: String -> Integer
        getNumber = read . takeWhile isDigit . dropWhile (not . isDigit)
        isFile = (`notElem` [".", ".."])

-- | Uses ImageMagick to join several images into pages of a PDF.
joinPages :: [FilePath] -> TempIO FilePath
joinPages fins = do
  cmd <- liftIO $ imageMagick' "convert"
  fout <- newTempFile "joinPages.pdf"
  void $ liftIO $ readProcess cmd (fins ++ [fout]) ""
  return fout
