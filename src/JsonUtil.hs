{-# LANGUAGE OverloadedStrings     #-}

module JsonUtil
  ( jsonOutput
  , jsonPutStr
  , hJsonPutStr
  , outputValue
  , UTCTime
  , fmtDateXmlSchema
  , fmtDateHTTP
  , pair
  , APair
  , A.toJSON
  , A.Value(..)
  , A.object
  , buildNOOP
  , fullWord
  )
where

import           Control.Exception          (bracket)
import           Data.Aeson                 (ToJSON, encode)
import qualified Data.Aeson                 as A (ToJSON, toJSON, object, Value(..))
import           Data.Aeson.Encode.Pretty   (Config(..), encodePretty', keyOrder)
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Lazy.Char8 as LC
import           Data.Monoid                (Monoid(..))
import           System.IO

import qualified Data.Text                  as T
import           Data.Text                  (Text)
import           Data.Time                  (UTCTime)
import           Data.Time.Format           (formatTime)
import           System.Locale              (defaultTimeLocale)

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

--- Command building utilities

fmtDateXmlSchema :: UTCTime -> String
fmtDateXmlSchema = fmtDate' "%FT%X"

fmtDateHTTP :: UTCTime -> String
fmtDateHTTP = fmtDate' "%a %b %e %H:%M:%S %Z %Y"

fmtDate' :: String -> UTCTime -> String
fmtDate' fmt
    = formatTime defaultTimeLocale fmt

pair :: A.ToJSON c => String -> c -> (Text, A.Value)
pair k v = (T.pack k, A.toJSON v)

type APair = (Text, A.Value)

buildNOOP :: A.Value
buildNOOP = A.object [ pair "cmd" ("noop" :: String) ]

-- Build a case-fullword clause for a query.
fullWord s = A.object [ pair "op"   ("case" :: String)
                      , pair "type" ("fullword" :: String)
                      , pair "word" s
                      ]
