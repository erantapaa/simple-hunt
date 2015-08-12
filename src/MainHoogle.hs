
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
import qualified Data.ByteString.Lazy.Char8 as LBS
import           TarUtil (pipesTarEntries)
 
checkFileExists path = do
  ok <- doesFileExist path
  if not ok
    then error $ "file does not exist: " ++ path
    else return ()

-- process a list of Hoogle files
processHoogleFiles :: FilePath -> Bool -> [FilePath] -> IO ()
processHoogleFiles scorePath emitDeleteCmd paths = do
  checkFileExists scorePath
  Just scoreMap <- readScores scorePath
  let scoreFn = \pkgName -> Map.lookup pkgName scoreMap
  now <- getCurrentTime
  forM_ paths $ \path -> do
    putStrLn $ "processing " ++ path
    processHoogleFile scoreFn now emitDeleteCmd path

-- Process a single Hoogle file
processHoogleFile scoreFn now emitDeleteCmd path = do
  let pkgName = dropExtension $ takeBaseName path
      jsonPath = "json/" ++ pkgName ++ ".js"
  fh <- openFile jsonPath WriteMode
  hPutStrLn fh "["
  if emitDeleteCmd
    then J.hJsonPutStr True fh (FC.buildDelete pkgName)
    else J.hJsonPutStr True fh J.buildNOOP
  evalHState $ skipHeader path
                 >-> toHoogleLine
                 >-> toFunctionInfo
                 >-> toCommands scoreFn now
                 >-> emitCommaJson fh
  hPutStrLn fh "]"

-- Process all Hoogle files in a tar archive.
processHoogleTarArchive scorePath now emitDeleteCmd path = do
  Just scoreMap <- readScores scorePath
  let scoreFn = \pkgName -> Map.lookup pkgName scoreMap
  now <- getCurrentTime
  runEffect $ pipesTarEntries path >-> for cat go1 >-> for cat (go2 scoreFn now)
  where
    go1 ent = do
      case parseTarEntry ent of
        Nothing              -> return ()
        Just (name, content) -> yield (name, content)
    go2 scoreFn now (pkgName, content) = lift $ do
      let jsonPath = "json/" ++ pkgName ++ ".js"
      fh <- openFile jsonPath WriteMode
      hPutStrLn fh "["
      if emitDeleteCmd
        then J.hJsonPutStr True fh (FC.buildDelete pkgName)
        else J.hJsonPutStr True fh J.buildNOOP
      evalHState $ skipHeaderLBS content
                     >-> toHoogleLine
                     >-> toFunctionInfo
                     >-> toCommands scoreFn now
                     >-> emitCommaJson fh
      hPutStrLn fh "]"

parseTarEntry :: Tar.Entry -> Maybe (String, LBS.ByteString)
parseTarEntry ent = do
  (content, len) <- getNormalFileContent $ Tar.entryContent ent
  -- XXX make sure file name has extension .txt?
  let name = dropExtension (takeBaseName (Tar.entryPath ent))
  return $ (name, content)
  where
    getNormalFileContent (Tar.NormalFile content len) = Just (content,len)
    getNormalFileContent _                            = Nothing

{-
processHoogleContent scoreFn now emitDeleteCmd pkgName content = do
  let jsonPath = "json/" ++ pkgName ++ ".js"
  fh <- openFile jsonPath WriteMode
  hPutStrLn fh "["
  if emitDeleteCmd
    then J.hJsonPutStr True fh (FC.buildDelete pkgName)
    else J.hJsonPutStr True fh J.buildNOOP
  evalHState $ skipHeader path
                 >-> toHoogleLine
                 >-> toFunctionInfo
                 >-> toCommands scoreFn now
                 >-> emitCommaJson fh
  hPutStrLn fh "]"
-}

