{-# LANGUAGE NoMonomorphismRestriction, DeriveDataTypeable, MultiWayIf #-}

module TarUtil where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Data.ByteString.Lazy as LBS

import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.BZip as BZip

import Data.List (isSuffixOf, unfoldr)
import qualified Data.Version as V

import Text.ParserCombinators.ReadP

import Control.Monad
import Control.Exception
import Data.Typeable

import Pipes
import qualified Pipes.Parse as P

import Text.Show.Pretty

data TarException = TarFormatError Tar.FormatError
  deriving (Show, Typeable)

instance Exception TarException where

tarEntriesForPath path = do
  let decompress
        | isSuffixOf ".bz2" path = BZip.decompress
        | isSuffixOf ".gz" path  = GZip.decompress
        | isSuffixOf ".tgz" path = GZip.decompress
        | otherwise              = id
  fmap (Tar.read . decompress) $ LBS.readFile path

pathParts path = unfoldr go path
  where go [] = Nothing
        go p  = case span (/= '/') p of
                 ([], [])     -> Nothing
                 ([], rest)   -> go (tail rest)
                 (leaf, rest) -> Just (leaf, rest)

hasThreeParts path =
 case pathParts path of
   (pkg : version : cabalname : _) -> Just (pkg, version, cabalname)
   _                               -> Nothing

firstParse :: ReadP a -> String -> Maybe a
firstParse readp str =
  case (readP_to_S readp) str of
    []        -> Nothing
    ((v,_):_) -> Just v

parseVersion :: ReadP V.Version
parseVersion = do v <- V.parseVersion; eof; return v

parsePath :: String -> Maybe (String, V.Version, String)
parsePath path = do
  (pkg, vers, cabal) <- hasThreeParts path
  version <- firstParse parseVersion vers
  return $ (pkg, version, cabal)

data ParsedEntry = ParsedEntry { pe_package   :: String
                               , pe_version   :: V.Version
                               , pe_cabalname :: String
                               , pe_content   :: LBS.ByteString
                               , pe_size      :: Tar.FileSize
                               }

instance Show ParsedEntry where
  show pe = "ParsedEntry { pe_package = " ++ show (pe_package pe)
            ++ ", pe_version = " ++ show (pe_version pe)
            ++ ", pe_cabalname = " ++ show (pe_cabalname pe)
            ++ ", pe_size = " ++ show (pe_size pe)
            ++ " }"

parseCabalEntry :: Tar.Entry -> Maybe ParsedEntry
parseCabalEntry ent = do
  (content, len) <- getNormalFileContent $ Tar.entryContent ent
  (pkg, vers, cabalname) <- hasThreeParts $ Tar.entryPath ent
  version <- firstParse parseVersion vers
  return $ ParsedEntry pkg version cabalname content len
  where
    getNormalFileContent (Tar.NormalFile content len) = Just (content,len)
    getNormalFileContent _                            = Nothing

-- | Create a stream of Tar.Entry; signal the end with Nothing
pipesTarEntries entries = 
  case entries of
    Tar.Next ent next -> do yield (Just ent); pipesTarEntries next
    Tar.Done          -> yield Nothing
    Tar.Fail e        -> throw (TarFormatError e)

-- | Filter a stream of Tar.Entry to only include the cabal files
pipesSelectCabals = forever $ do
  ment <- await
  case ment of
    Nothing  -> yield Nothing
    Just ent -> let mr = parseCabalEntry ent in
                case mr of
                  Nothing -> return ()
                  Just r  -> yield (Just r)

-- | Filter only the latest versions of each cabal file.
-- Assume file names are in sorted order.
pipesLatestVersions = do
  let loop0 = do
        mnew <- await
        case mnew of
          Nothing -> return ()
          Just x  -> loop x
      loop old = do
        mnew <- await
        case mnew of
          Nothing -> yield old
          Just new -> if | pe_package old /= pe_package new -> do yield old; loop new
                         | pe_version old < pe_version new  -> loop new
                         | otherwise                        -> loop old
  loop0

showParsedEntry pe = do
  putStrLn $ ppShow pe

pipesShowEntries = forever $ do
  pe <- await
  lift $ showParsedEntry pe

-- print only the latest versions of each cabal file in an archive
test1 path = do
  entries <- tarEntriesForPath path 
  runEffect $ pipesTarEntries entries >-> pipesSelectCabals >-> pipesLatestVersions >-> pipesShowEntries

