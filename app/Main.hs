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

main :: IO ()
main = do 
  cabal <- getContents
  case parseCabal cabal of
    Nothing -> error "oops"
    Just pkgInfo -> do
      now <- getCurrentTime
      let pkgName = p_name pkgInfo
      let cmd = toCommand True now True [(pkgName, Just pkgInfo)]
      outputValue "output.json" cmd
      return ()
      -- toCommand :: Bool -> UTCTime -> Bool -> [(String, Maybe PackageInfo)] -> Command 

