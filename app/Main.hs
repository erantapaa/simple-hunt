module Main where

import qualified MainCabal as MC
import qualified MainHoogle as MH

main = do
  MC.main "index.tar.gz"
  MH.main "hoogle.tar.gz"

