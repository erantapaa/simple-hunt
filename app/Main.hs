module Main where

import qualified MainCabal as MC
import qualified MainHoogle as MH
import Data.Time.Clock (getCurrentTime)

main = do
  MC.main "index.tar.gz"
  now <- getCurrentTime
  MH.processHoogleTarArchive "json/02-ranking.js" now True "hoogle.tar.gz"

