{-# LANGUAGE Rank2Types #-}

module MainHoogle where

import           Control.Monad
import qualified Data.Map         as Map
import           Data.Time.Clock (getCurrentTime)
import           ProcessHoogle  (skipHeader, skipHeaderLBS, evalHState, toHoogleLine, toCommands, emitCommaJson, toFunctionInfo, readScores)
import qualified ProcessLine      as PL
import qualified FctIndexerCore   as FC
import qualified JsonUtil         as J
import           Pipes
import           System.IO
import           System.Directory
import           System.FilePath
import qualified Codec.Archive.Tar as Tar
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           TarUtil (tarEntriesForPath, pipesTarEntries)
import qualified Data.Text as Text

import Control.Monad.Trans.State.Strict
 
checkFileExists path = do
  ok <- doesFileExist path
  when (not ok) $ error $ "file does not exist: " ++ path

type LinesProducer = MonadIO m => Producer (Int, Text.Text) m ()

-- process a Hoogle file represetned as stream of strict ByteStrings.

processHoogle :: (String -> Maybe Float)   -- ^ Score function
              -> J.UTCTime                 -- ^ Index time
              -> [ J.Value ]               -- ^ Commands to emit before inserts
              -> LinesProducer             -- ^ Lines of the hoogle file
              -> Handle                    -- ^ Output file handle
              -> IO ()
processHoogle scoreFn now deleteCmds linesStream fh = do
  hPutStrLn fh "["
  if null deleteCmds
    then J.hJsonPutStr True fh J.buildNOOP
    else forM_ deleteCmds $ J.hJsonPutStr True fh
  evalHState $ linesStream
                 >-> toHoogleLine
                 >-> toFunctionInfo
                 >-> toCommands scoreFn now
                 >-> emitCommaJson fh
  hPutStrLn fh "]"


-- process a list of Hoogle files
processHoogleFiles :: FilePath -> Bool -> [FilePath] -> IO ()
processHoogleFiles scorePath emitDeleteCmd paths = do
  checkFileExists scorePath
  Just scoreMap <- readScores scorePath
  let scoreFn = \pkgName -> Map.lookup pkgName scoreMap
  now <- getCurrentTime
  forM_ paths $ \path -> do
    putStrLn $ "processing " ++ path
    let pkgName = dropExtension $ takeBaseName path
        jsonPath = "json/" ++ pkgName ++ ".js"
        deletes = if emitDeleteCmd then [ FC.buildDelete pkgName ] else []
    withFile jsonPath WriteMode $ processHoogle scoreFn now deletes (skipHeader path)

-- Process all Hoogle files in a tar archive.
processHoogleTarArchive scorePath now emitDeleteCmd path = do
  Just scoreMap <- readScores scorePath
  let scoreFn = \pkgName -> Map.lookup pkgName scoreMap
  entries <- tarEntriesForPath path
  runEffect $ pipesTarEntries entries >-> for cat go1 >-> for cat (go2 scoreFn now)
  where
    go1 ent = do
      case parseTarEntry ent of
        Nothing              -> return ()
        Just (name, content) -> yield (name, content)
    go2 scoreFn now (pkgName, content) = lift $ do
      let jsonPath = "json/" ++ pkgName ++ ".js"
          deletes = if emitDeleteCmd then [ FC.buildDelete pkgName ] else []
      liftIO $ withFile jsonPath WriteMode $ processHoogle scoreFn now deletes (skipHeaderLBS content)

-- Select only normal files from a tar archive.
parseTarEntry :: Tar.Entry -> Maybe (String, LBS.ByteString)
parseTarEntry ent = do
  (content, len) <- getNormalFileContent $ Tar.entryContent ent
  -- XXX make sure file name has extension .txt?
  let name = dropExtension (takeBaseName (Tar.entryPath ent))
  return $ (name, content)
  where
    getNormalFileContent (Tar.NormalFile content len) = Just (content,len)
    getNormalFileContent _                            = Nothing

main hoogleTarPath = do
  now <- getCurrentTime
  processHoogleTarArchive "json/02-ranking.js" now True hoogleTarPath

