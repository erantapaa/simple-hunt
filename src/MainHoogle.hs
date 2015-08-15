{-# LANGUAGE Rank2Types, NoMonomorphismRestriction #-}

module MainHoogle where

import           Control.Monad
import qualified Data.Map           as Map
import           Data.Time.Clock    (getCurrentTime)
import           ProcessHoogle      (skipHeader, skipHeaderLBS, evalHState, toHoogleLine, toCommands, emitCommaJson, toFunctionInfo, readScores)
import qualified FctIndexerCore     as FC
import           Hayoo.FunctionInfo (FunctionInfo(..))
import qualified JsonUtil           as J

import           Pipes
import qualified Pipes.Group        as PG
import qualified Pipes.Prelude      as P
import qualified Pipes.Parse        as PP
import qualified Pipes.Lift         as PL

import qualified Data.Set           as Set

import           Control.Lens       (view, zoom, (^.))

import           System.IO
import           System.Directory
import           System.FilePath

import qualified Codec.Archive.Tar  as Tar
import           TarUtil            (tarEntriesForPath, pipesTarEntries)

import           Data.ByteString    (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text          as Text

import Control.Monad.State.Strict

type LinesProducer = MonadIO m => Producer (Int, Text.Text) m ()

checkFileExists path = do
  ok <- doesFileExist path
  when (not ok) $ error $ "file does not exist: " ++ path

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

processHoogle' :: (String -> Maybe Float)   -- ^ Score function
               -> J.UTCTime                 -- ^ Index time
               -> LinesProducer             -- ^ Lines of the hoogle file
               -> Handle                    -- ^ Output file handle
               -> IO ()
processHoogle' scoreFn now linesStream fh = do
  evalHState $ linesStream
                 >-> toHoogleLine
                 >-> toFunctionInfo
                 >-> removeDupURIs
                 >-> toCommands scoreFn now
                 >-> emitCommaJson fh

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

-- return a producer which emits tar entries for just normal files
filesInTarArchive path = do
  entries <- tarEntriesForPath path
  return $ pipesTarEntries entries >-> for cat onlyFiles
    where onlyFiles = maybe (return ()) yield . parseTarEntry

-- filter out FunctionInfo records with the same docURI.

removeDupURIs = PL.evalStateP Set.empty (for cat go)
  where go item@(name, fi) = do
          seen <- get
          let uri = docURI fi
          if Set.member uri seen
            then return ()
            else do put (Set.insert uri seen)
                    yield item

mapParserGroups nextProducer run = loop (1::Int)
 where
   loop n = do
     end <- PP.isEndOfInput
     if end
       then return ()
       else do s <- get
               let x = s ^. nextProducer
               y <- lift $ run n x
               put y
               loop (n+1)

processHoogleBatched scoreFn now batchSize hoogleTarPath = do
  files <- filesInTarArchive hoogleTarPath
  let nextProducer = PP.splitAt batchSize
      run          = processFileStream scoreFn now
  evalStateT (mapParserGroups nextProducer run) files

processFileStream scoreFn now n x = do
  let out = "json/batch-" ++ show n ++ ".js"
  putStrLn $ "writing to " ++ out

  let go h (pkgName,content) = do
        --- emit the delete command
        let deleteCmd = FC.buildDelete pkgName
        lift $ do putStrLn $ " - " ++ pkgName
                  J.hJsonPutStr True h deleteCmd
                  processHoogle' scoreFn now (skipHeaderLBS content) h

  withFile out WriteMode $ \h -> do
    hPutStrLn h "["
    y <- runEffect $ (x >-> for cat (go h))   -- process the batch
    hPutStrLn h "]"
    return y

noBuffering = hSetBuffering stdout NoBuffering

test6 = do
  noBuffering
  let scorePath = "json/02-ranking.js"
  now <- getCurrentTime
  Just scoreMap <- readScores scorePath
  let scoreFn = \pkgName -> Map.lookup pkgName scoreMap
  processHoogleBatched scoreFn now 20 "hoogle.tar.gz"

main hoogleTarPath = do
  let scorePath = "json/02-ranking.js"
      hooglePath = "hoogle.tar.gz"
      batchSize = 20
  noBuffering
  now <- getCurrentTime
  Just scoreMap <- readScores scorePath
  let scoreFn = \pkgName -> Map.lookup pkgName scoreMap
  processHoogleBatched scoreFn now batchSize hooglePath

