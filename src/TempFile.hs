-- | A wrapper around IO that allows you to treat temporary files like
-- garbage-collected values.
module TempFile
( TempIO
, runTempIO
, newTempFile
, ask
, liftIO
) where

import System.IO.Temp (openTempFile, withSystemTempDirectory)
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (liftIO)
import System.IO (hClose)
import System.Directory (copyFile, renameFile)
import System.IO.Error (catchIOError)

type TempIO = ReaderT FilePath IO

runTempIO :: FilePath -> TempIO FilePath -> IO ()
runTempIO fout act = withSystemTempDirectory "tempfile" $ \tmp -> do
  res <- runReaderT act tmp
  catchIOError (renameFile res fout) $ \_ -> copyFile res fout

newTempFile :: String -> TempIO FilePath
newTempFile pat = do
  tmp <- ask
  (f, h) <- liftIO $ openTempFile tmp pat
  liftIO $ hClose h
  return f
