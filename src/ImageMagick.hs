module ImageMagick
( imageMagick
, connectVertical
, splitVertical
, joinPages
)
where

import System.Directory (getDirectoryContents)
import System.Process (readProcess, readProcessWithExitCode)
import System.FilePath ((</>))
import System.Exit (ExitCode(..))
import Control.Applicative ((<$>))
import Control.Monad (void)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.List (isPrefixOf)
import System.Environment (lookupEnv)
import qualified System.Info as Info

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
