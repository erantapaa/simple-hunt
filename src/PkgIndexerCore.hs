{-# LANGUAGE OverloadedStrings #-}

module PkgIndexerCore
where

-- import Debug.Trace

import           Control.Applicative          ((<$>))
import           Control.DeepSeq              (NFData, rnf)

import           Data.Aeson                   (encode)
-- import           Data.Binary                  (Binary)
-- import qualified Data.Binary                  as B
-- import qualified Data.StringMap.Strict        as M
import qualified Data.Text                    as T
import           Data.Time                    (UTCTime)

import           Hayoo.Hunt.ApiDocument       (piToDescr)

import           Hayoo.Hunt.IndexSchema       (appendSaveCmd,
                                               c'type, c'indexed, c'name, c'partial, c'upload,
                                               c'author, c'category, c'synopsis, c'description,
                                               c'dependencies,
                                               d'indexed, d'package,
                                               fmtDateXmlSchema, fmtDateHTTP, parseDateHTTP
                                              )

import           Hunt.ClientInterface          hiding (URI)
import           Hunt.Common.ApiDocument       (ApiDocument)

import           Control.Arrow                 ((***))
-- import           Text.XML.HXT.Core             (IOSArrow, SysConfig, XmlTree, (***))

import qualified Hunt.Common.ApiDocument       as H
import qualified Data.Text                     as Text
import           Data.Char
import qualified Data.Map.Lazy                 as LazyMap
import           PackageInfo                   (PackageInfo(..))
import           Hunt.Query.Language.Builder
import           Hunt.Interpreter.Command      (Command)

buildDocIndex :: UTCTime -> String -> PackageInfo -> H.IndexMap
buildDocIndex now pkgName pkgInfo = LazyMap.fromList $ 
     add c'author       (parseWords $ p_author pkgInfo)
  ++ add c'category     (parseWords $ p_category pkgInfo)
  ++ add c'dependencies (map Text.pack $ p_dependencies pkgInfo)
  ++ add c'description  (parseWords $ p_description pkgInfo)
  ++ add c'synopsis     (parseWords $ p_synopsis pkgInfo)
  ++ add c'name         [Text.pack pkgName]
  ++ add c'type         ["package"]
  ++ add c'indexed      [ now' ]
  ++ add c'upload       [ upl ]
  ++ add c'partial      [ ns ]
  where
    add keyword words =  [ (keyword, Text.unwords words) ]
    parseWords str = filter (not . Text.null) $ Text.split (not . isLetter) (Text.pack str)
    now'  = fmtDateXmlSchema now

    upl = maybe "" id uplDate
        where
          uplDate
              = do let dt1 = p_uploaddate pkgInfo
                   pd  <- parseDateHTTP dt1
                   return $ fmtDateXmlSchema pd

    names = T.words . Text.pack $ pkgName
    (n, ns) = (T.concat *** T.concat) . splitAt 1 $ names

toCommand :: Bool -> UTCTime -> Bool -> [(String, Maybe PackageInfo)] -> Command
toCommand save now update pkgs
      = appendSaveCmd save now $
        cmdSequence [ deletePkgCmd
                    , cmdSequence . concatMap toCmd $ [ (pkgName, pkgInfo) | (pkgName, Just pkgInfo) <- pkgs ]
                    ]

    where
      deletePkgCmd
          | update && not (null pkgs)
              = cmdDeleteDocsByQuery
                . qAnd ( setContext c'type
                         $ qFullWord d'package
                     )
                . setContext c'name
                . qOrs
                . map (\(t,_) -> qFullWord . T.pack $ t)
                $ pkgs

          | otherwise
              = cmdNOOP

      toCmd (pkgName, pkgInfo) = insertCmd apiDoc
          where 
              insertCmd = (:[]) . cmdInsertDoc
              apiDoc =  setDescription (piToDescr pkgInfo)
                        . setIndex     indexMap 
                        . setDocWeight 1.0
                        $ mkApiDoc uri
              uri = "http://hackage.haskell.org/package/aeson"
              indexMap = buildDocIndex now pkgName pkgInfo

