-- | A wrapper around IO that allows you to treat temporary files like
-- garbage-collected values.
module TempFile
( TempIO
, runTempIO
, newTempFile
, ask
, liftIO
) where

import System.IO (hClose)
import System.IO.Error (catchIOError)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask, ReaderT(..))
import System.Directory (copyFile, renameFile)
import System.IO.Temp (openTempFile, withSystemTempDirectory)

-- | A wrapper around IO with a designated directory for temporary files.
type TempIO = ReaderT FilePath IO

-- | Creates a new temporary directory to run a computation in. When finished,
-- the final file will be moved/copied to the given path, and the temporary
-- directory will be deleted.
runTempIO :: FilePath -> TempIO FilePath -> IO ()
runTempIO fout act = withSystemTempDirectory "tempfile" $ \tmp -> do
  res <- runReaderT act tmp
  catchIOError (renameFile res fout) $ \_ -> copyFile res fout

-- | Creates a new file in the temporary directory, given a template.
newTempFile :: String -> TempIO FilePath
newTempFile pat = do
  tmp <- ask
  (f, h) <- liftIO $ openTempFile tmp pat
  liftIO $ hClose h
  return f
