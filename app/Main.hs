{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Main2 as M2

{-
outputValue :: (ToJSON c) => String -> c -> IO ()
outputValue fn c = jsonOutput True toFile c
    where
      toFile bs
          = do createDirectoryIfMissing True dirPath
               LB.writeFile file bs
          where
            (dirPath, _) = splitFileName file
            file         = jsonPath fn
-}

main = M2.main14

