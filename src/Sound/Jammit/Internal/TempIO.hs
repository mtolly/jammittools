-- | A wrapper around IO that allows you to treat temporary files like
-- garbage-collected values.
module Sound.Jammit.Internal.TempIO
( TempIO
, runTempIO
, newTempFile
, ask
, liftIO
) where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (ReaderT (..), ask)
import           Data.List                  (stripPrefix)
import           System.Directory           (copyFile, renameFile)
import           System.FilePath            (splitPath)
import           System.IO                  (hClose)
import           System.IO.Error            (catchIOError)
import           System.IO.Temp             (openTempFile,
                                             withSystemTempDirectory)

-- | A wrapper around IO with a designated directory for temporary files.
type TempIO = ReaderT FilePath IO

-- | Creates a new temporary directory to run a computation in. When finished,
-- the final file will be moved/copied to the given path, and the temporary
-- directory will be deleted.
runTempIO :: FilePath -> TempIO FilePath -> IO ()
runTempIO fout act = withSystemTempDirectory "tempfile" $ \tmp -> do
  res <- runReaderT act tmp
  case stripPrefix (splitPath tmp) (splitPath res) of
    Just f | ".." `notElem` f -> -- try rename if we know the file is in tmp
      catchIOError (renameFile res fout) $ \_ -> copyFile res fout
    _ -> copyFile res fout

-- | Creates a new file in the temporary directory, given a template.
newTempFile :: String -> TempIO FilePath
newTempFile pat = do
  tmp <- ask
  (f, h) <- liftIO $ openTempFile tmp pat
  liftIO $ hClose h
  return f
