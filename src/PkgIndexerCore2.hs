module PkgIndexerCore2
where

import qualified Data.Aeson                   as A
import qualified Data.Vector                  as V
import qualified Data.Text                    as T
import           Data.Text                    (Text)

import           PackageInfo                  (PackageInfo(..))

import           Data.Time                    (UTCTime)
import           Data.Time.Format             (formatTime)
import           System.Locale                (defaultTimeLocale)
import           Data.Scientific              as S
import           Data.List                    (intercalate)

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

-- | Build the index for a PackageInfo (except for the "indexed" value)
buildIndexPairs :: String                       -- ^ Name of function / type / module / etc.
                -> PackageInfo                  -- ^ FunctionInfo record
                -> [(Text, A.Value)]            -- ^ Pairs comprising the index for this document
buildIndexPairs pkgName pkgInfo = kvpairs
  where
    kvpairs =
      -- description, author, category, indexed, name, synopsis, type, dependencies
      [ pair "description"  (p_description pkgInfo)
      , pair "author"       (p_author pkgInfo)
      , pair "category"     (p_category pkgInfo)
      , pair "name"         (p_name pkgInfo)
      , pair "synopsis"     (p_synopsis pkgInfo)
      , pair "type"         "package"
      , pair "dependencies" (intercalate " " (p_dependencies pkgInfo))
      ]

-- | Build the document component of an Insert command.
buildDocument :: UTCTime         -- ^ The indexed time
              -> String          -- ^ The function / method / type name
              -> PackageInfo     -- ^ The FunctionInfo record
              -> A.Value         -- ^ Document object (as JSON)
buildDocument now pkgName pkgInfo =
  A.object  $
  [ pair "description"
         (A.object $ [ -- description, author, category, dependencies, indexed, maintainer, name, synopsis
                       -- type, version
                       pair "indexed"      nowD
                     , pair "description"  (p_description pkgInfo)
                     , pair "author"       (p_author pkgInfo)
                     , pair "category"     (p_category pkgInfo)
                     , pair "dependencies" (p_dependencies pkgInfo)
                     , pair "maintainer"   (p_maintainer pkgInfo)
                     , pair "name"         (p_name pkgInfo)
                     , pair "synopsis"     (p_synopsis pkgInfo)
                     , pair "version"      (p_version pkgInfo)
                     ]
         )
  , pair "index"       index
  , pair "uri"         uri
  ]
  where
    nowD     = A.toJSON $ fmtDateHTTP now       -- date formatted for the document
    nowI     = A.toJSON $ fmtDateXmlSchema now  -- date formatted for the index
    uri      = "http://hackage.haskell.org/package/" ++ (p_name pkgInfo)
    index    = A.object $ [ pair "indexed" nowI ] ++ buildIndexPairs pkgName pkgInfo

-- | Build the Delete command for a list of packages.
buildDeletes :: [String] -> A.Value
buildDeletes pkgNames =
  A.object
  [ pair "cmd"   "delete-by-query"
  , pair "query" (A.object [ pair "args" [arg1, arg2]
                           , pair "op"   "and"
                           , pair "type" "seq"
                           ]
                 )
  ]
  where arg1 = A.object [ pair "type"    "context"
                        , pair "contexts" [ "type" ]
                        , pair "query"    (fullWord "package")
                        ]
        arg2 = A.object [ pair "type"     "context"
                        , pair "contexts" [ "name" ]
                        , pair "query"    (A.object [ pair "args" (map fullWord pkgNames)
                                                   , pair "op"   "or"
                                                   , pair "type" "seq"
                                                   ]
                                          )
                        ] 
        fullWord s = A.object [ pair "op"   "case"
                              , pair "type" "fullword"
                              , pair "word" s
                              ]

buildNOOP :: A.Value
buildNOOP = A.object [ pair "cmd" "noop" ]

