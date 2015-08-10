{-# LANGUAGE OverloadedStrings     #-}

module JsonUtil
  ( jsonOutput
  , jsonPutStr
  , hJsonPutStr
  , outputValue
  )
where

import           Control.Exception          (bracket)
import           Data.Aeson                 (ToJSON, encode)
import           Data.Aeson.Encode.Pretty   (Config(..), encodePretty', keyOrder)
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Lazy.Char8 as LC
import           Data.Monoid                (Monoid(..))
import           System.IO

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

jsonPutStr :: (ToJSON c) => Bool -> c -> IO ()
jsonPutStr pretty c = jsonOutput pretty LC.putStrLn c

hJsonPutStr pretty h c = jsonOutput pretty (LC.hPutStrLn h) c

outputValue :: ToJSON c => FilePath -> c -> IO ()
outputValue path c = bracket (openBinaryFile path WriteMode) hClose (\h -> hJsonPutStr True h c)

