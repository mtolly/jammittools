module Sound.Jammit.Internal.Sox
( Audio(..)
, Time(..)
, renderAudio
, optimize
) where

import Control.Arrow (first)
import Control.Applicative ((<$>))
import Control.Monad (forM, guard, void)
import Data.List (isPrefixOf)
import Data.Maybe (listToMaybe)
import System.Environment (lookupEnv)
import qualified System.Info as Info

import System.Directory (getDirectoryContents, findExecutable)
import System.FilePath ((</>))
import System.Process (readProcess)

import Sound.Jammit.Internal.TempFile

data Audio
  = Empty                 -- ^ An empty stereo file
  | File FilePath         -- ^ An existing (stereo) file
  | Pad Time Audio        -- ^ Pad audio start with silence
  | Mix [(Double, Audio)] -- ^ Add audio sample-wise, also multiplying volumes
  | Concat [Audio]        -- ^ Sequentially connect audio
  deriving (Eq, Ord, Show, Read)

data Time
  = Seconds Double
  | Samples Integer
  deriving (Eq, Ord, Show, Read)

showTime :: Time -> String
showTime (Seconds d) = show d
showTime (Samples i) = show i ++ "s"

renderAudio :: Audio -> TempIO FilePath
renderAudio aud = case aud of
  Empty -> do
    fout <- newTempFile "render.wav"
    liftIO $ runSox $ ["-n", fout] ++ words "trim 0 0 channels 2"
    return fout
  File f -> return f
  Pad t x -> do
    fin <- renderAudio x
    fout <- newTempFile "render.wav"
    liftIO $ runSox [fin, fout, "pad", showTime t]
    return fout
  Mix xs -> case xs of
    [] -> renderAudio Empty
    [(d, x)] -> do
      fin <- renderAudio x
      fout <- newTempFile "render.wav"
      liftIO $ runSox ["-v", show d, fin, fout]
      return fout
    _ -> do
      dfins <- forM xs $ \(d, x) -> do
        fin <- renderAudio x
        return (d, fin)
      let argsin = concatMap
            (\(d, fin) -> ["-v", show d, fin]) dfins
      fout <- newTempFile "render.wav"
      liftIO $ runSox $ ["--combine", "mix"] ++ argsin ++ [fout]
      return fout
  Concat xs -> case xs of
    [] -> renderAudio Empty
    _ -> do
      fins <- mapM renderAudio xs
      fout <- newTempFile "render.wav"
      liftIO $ runSox $ fins ++ [fout]
      return fout

optimize :: Audio -> Audio
optimize aud = case aud of
  Pad (Samples 0) x -> x
  Pad (Seconds 0) x -> x
  Mix xs -> let
    xs' = do
      (d, x) <- xs
      guard $ d /= 0
      case optimize x of
        Mix ys -> map (first (* d)) ys
        x'     -> [(d, x')]
    in case xs' of
      []       -> Empty
      [(1, x)] -> x
      _        -> Mix xs'
  Concat xs -> let
    xs' = do
      x <- xs
      case optimize x of
        Concat ys -> ys
        x'        -> [x']
    in case xs' of
      []  -> Empty
      [x] -> x
      _   -> Concat xs'
  _ -> aud

runSox :: [String] -> IO ()
runSox args = do
  sox <- findSox
  case sox of
    Just prog -> void $ readProcess prog args ""
    Nothing   -> error "runSox: couldn't find sox executable"

-- | Find the SoX binary on Windows in case it's not in the PATH.
findSox :: IO (Maybe String)
findSox = do
  inPath <- findExecutable "sox"
  case inPath of
    Just prog -> return $ Just prog
    Nothing -> case Info.os of
      "mingw32" -> firstJustM $
        -- env variables for different configs of (ghc arch)/(sox arch)
        -- ProgramFiles: 32/32 or 64/64
        -- ProgramFiles(x86): 64/32
        -- ProgramW6432: 32/64
        flip map ["ProgramFiles", "ProgramFiles(x86)", "ProgramW6432"] $ \env ->
          lookupEnv env >>= \var -> case var of
            Nothing -> return Nothing
            Just pf
              ->  fmap (\im -> pf </> im </> "sox.exe")
              .   listToMaybe
              .   filter ("sox-" `isPrefixOf`)
              <$> getDirectoryContents pf
      _ -> return Nothing

-- | Only runs actions until the first that gives 'Just'.
firstJustM :: (Monad m) => [m (Maybe a)] -> m (Maybe a)
firstJustM [] = return Nothing
firstJustM (mx : xs) = mx >>= \x -> case x of
  Nothing -> firstJustM xs
  Just y  -> return $ Just y
