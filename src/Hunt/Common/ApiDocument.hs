{-# LANGUAGE OverloadedStrings #-}

module Hunt.Common.ApiDocument

where

import           Control.Applicative
import           Control.Monad          (mzero)
import           Control.DeepSeq
import           Data.Aeson
import           Data.Binary            (Binary (..))
import           Data.Map.Strict        (Map ())
import qualified Data.Map.Strict        as M
import           Data.Text              (Text)
import qualified Data.Text              as T

import qualified Hunt.Common.DocDesc    as DD

-- | The document accepted by the interpreter and JSON API.

data ApiDocument  = ApiDocument
    { adUri   :: Text             -- ^ The unique identifier.
    , adIndex :: IndexMap         -- ^ The data to index according to schema associated with the context.
    , adDescr :: Description      -- ^ The document description (a simple key-value map).
    , adWght  :: Score            -- ^ An optional document boost, (internal default is @1.0@).
    }
    deriving (Show)

-- | Score
type Score = Float

type Context      = Text
type Content      = Text
type Description  = DD.DocDesc

-- | Context map
type IndexMap = Map Context Content

-- | Multiple 'ApiDocument's.
type ApiDocuments = [ApiDocument]

-- | Empty index content.
emptyApiDocIndexMap :: IndexMap
emptyApiDocIndexMap = M.empty

-- | Empty 'Document' description.
emptyApiDocDescr :: Description
emptyApiDocDescr = DD.empty

-- | Empty 'ApiDocument'.
emptyApiDoc :: ApiDocument
emptyApiDoc = ApiDocument "" emptyApiDocIndexMap emptyApiDocDescr noScore

instance NFData ApiDocument where
  --default

noScore :: Score
noScore = 0.0

mkScore :: Score -> Score
mkScore s = max 0 s

getScore :: Score -> Maybe Score
getScore s | s > 0     = Just s
           | otherwise = Nothing


instance FromJSON ApiDocument where
  parseJSON (Object o) = do
    parsedUri <- o    .: "uri"
    indexMap  <- o    .:? "index"       .!= emptyApiDocIndexMap
    descrMap  <- o    .:? "description" .!= emptyApiDocDescr
    weight    <- mkScore <$>
                 o    .:? "weight"      .!= 0.0
    return ApiDocument
      { adUri    = parsedUri
      , adIndex  = indexMap
      , adDescr  = descrMap
      , adWght   = weight
      }
  parseJSON _ = mzero

instance ToJSON ApiDocument where
  toJSON (ApiDocument u im dm wt)
      = object $
        ( maybe [] (\ x -> ["weight" .= x]) $ getScore wt )
        ++
        ( if M.null im
          then []
          else ["index" .= im]
        )
        ++
        ( if DD.null dm
          then []
          else ["description" .= dm]
        )
        ++
        ["uri" .= u]
