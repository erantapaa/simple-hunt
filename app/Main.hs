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
import           Data.Aeson.Encode.Pretty   (Config(..), encodePretty', keyOrder)
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Lazy.Char8 as LC
import           Data.Monoid

import           System.Environment
import TarUtil
import Pipes
import Text.Printf
import qualified Data.Text.Lazy.Encoding  as LE
import qualified Data.Text.Lazy as LT
import System.Exit
import Data.Time.Clock
import Control.Monad

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

processEntry now i = do
  pe <- await
  case LE.decodeUtf8' (pe_content pe) of
    Left e    -> lift $ putStrLn $ "utf8 decode error on " ++ pe_cabalname pe
    Right txt -> case parseCabal (LT.unpack txt) of
                   Nothing -> do lift $ putStrLn $ "error parse cabal for " ++ pe_cabalname pe
                                 processEntry now i
                   Just pkgInfo -> do let pkgName = p_name pkgInfo
                                          cmd = toCommand False now True [(pkgName, Just pkgInfo)]
                                          path = printf "out-%05d" (i::Int)
                                      lift $ outputValue path cmd
                                      lift $ putStrLn $ "processed " ++ pkgName
                                      processEntry now (i+1)

main :: IO ()
main = do 
  args <- getArgs
  when (null args) $ do
    putStrLn $ "Usage: app index.tar.gz"
    exitFailure
  let path = head args
  entries <- tarEntriesForPath path
  now <- getCurrentTime
  runEffect $ pipesTarEntries entries >-> pipesSelectCabals >-> pipesLatestVersions >-> processEntry now 1 -- pipesShowEntries
  end <- getCurrentTime
  let diff = diffUTCTime end now
  putStrLn $ "time: " ++ show diff
  exitSuccess
