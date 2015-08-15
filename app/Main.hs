module Main where

import qualified MainCabal as MC
import qualified MainHoogle as MH
import Paths_simple_hunt
import System.Directory (copyFile, createDirectoryIfMissing)
import System.IO

main = do
  hSetBuffering stdout NoBuffering
  -- create json/00-schema.js
  schemaPath <- getDataFileName "00-schema.js"
  createDirectoryIfMissing True "json"
  copyFile schemaPath "json/00-schema.json"
  putStrLn "copied 00-schema.json"
  -- create 01-packages.js and 02-ranking.js
  MC.main "index.tar.gz"
  MH.main "hoogle.tar.gz"

