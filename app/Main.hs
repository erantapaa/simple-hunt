{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import ParseCabal
import PackageInfo
import PkgIndexerCore (toCommand)
import           Data.Time                  (getCurrentTime)

import           System.Directory           (createDirectoryIfMissing)
import           System.FilePath            ((</>), (<.>), splitFileName)
import           Data.Aeson                 (ToJSON, decode, encode)
import qualified Data.Aeson                 as A
import           Data.Aeson.Encode.Pretty   (Config(..), encodePretty', keyOrder)
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Lazy.Char8 as LC
import           Data.Monoid
import qualified Data.Map                   as Map
import qualified Data.Scientific            as S

import           System.Environment
import TarUtil
import Pipes
import qualified Pipes.Prelude as Pipes
import Text.Printf
import qualified Data.Text                  as Text
import qualified Data.Text.Lazy.Encoding  as LE
import qualified Data.Text.Lazy as LT
import System.Exit
import Data.Time.Clock
import Control.Monad
import Hayoo.PackageRank

-- ------------------------------------------------------------

outputValue :: (ToJSON c) => String -> c -> IO ()
outputValue fn c = jsonOutput True toFile c
    where
      toFile bs
          = do createDirectoryIfMissing True dirPath
               LB.writeFile file bs
          where
            (dirPath, _) = splitFileName file
            file         = jsonPath fn

jsonOutput :: (ToJSON c) => Bool -> (LB.ByteString -> IO a) -> c -> IO a
jsonOutput pretty io x
    = io $ (if pretty then encodePretty' encConfig else encode) x
      where
        encConfig :: Config
        encConfig
            = Config { confIndent = 2
                     , confCompare
                         = keyOrder ["description", "index", "uri"]
                           `mappend`
                           compare
                     }

jsonPath :: String -> String
jsonPath fn = "json" </> fn <.> "js"

updateRankCmd uri weight =
  A.object [ ("cmd", A.String "update")
           , ("document", A.object [ ("uri", A.String uri), ("weight", A.Number weight) ] )
           ]

processEntry now i pe = do
  case LE.decodeUtf8' (pe_content pe) of
    Left e    -> do putStrLn $ "utf8 decode error on " ++ pe_cabalname pe
                    return Nothing
    Right txt -> case parseCabal (LT.unpack txt) of
                   Nothing -> do putStrLn $ "error parse cabal for " ++ pe_cabalname pe
                                 return Nothing
                   Just pkgInfo -> do let pkgName = p_name pkgInfo
                                          cmd = toCommand False now True [(pkgName, Just pkgInfo)]
                                          path = printf "out-%05d" (i::Int)
                                      outputValue path cmd
                                      putStrLn $ "processed " ++ pkgName
                                      return $ Just (p_name pkgInfo, p_dependencies pkgInfo)

main :: IO ()
main = do 
  args <- getArgs
  when (null args) $ do
    putStrLn $ "Usage: app index.tar.gz"
    exitFailure
  let path = head args
  entries <- tarEntriesForPath path
  now <- getCurrentTime
  entries <- Pipes.toListM $ pipesTarEntries entries >-> pipesSelectCabals >-> pipesLatestVersions
  let go (dag,i) pe = do r <- processEntry now i pe
                         case r of 
                           Nothing    -> return (dag, i)
                           Just edges -> return (edges:dag, i+1)
  (dag,_) <- foldM go ([],1) entries
  end1 <- getCurrentTime
  let diff = diffUTCTime end1 now
  putStrLn $ "time to process cabals: " ++ show diff
  putStrLn $ "computing ranks..."
  let ranks = rankingStd dag 
      toCmd pkg w = updateRankCmd (Text.pack uri) (S.fromFloatDigits w)
        where uri = "http://hackage.haskell.org/package/" ++ pkg
      rankcmds = map (uncurry toCmd) (Map.assocs ranks)
  outputValue "02-ranking" rankcmds
  end2 <- getCurrentTime
  putStrLn $ "time to write ranks: " ++ (show $ diffUTCTime end2 end1)
  exitSuccess
