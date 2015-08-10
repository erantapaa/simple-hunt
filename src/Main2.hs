{-# LANGUAGE NoMonomorphismRestriction #-}

module Main2
where

import Pipes 
import Pipes.Group
import Pipes.Prelude as P
import Control.Lens (view)

foldValues :: (Monad m, Eq k) => (v -> v -> v) -> Producer (k, v) m r -> Producer (k, v) m r
foldValues append xs =
    P.concat <-< folds step Nothing id (view (groupsBy keyEq) xs)
  where
    keyEq (k, _) (k', _) = k == k'

    step (Nothing)      (k, v) = Just (k, v)
    step (Just (_, v0)) (k, v) = Just (k, v0 `append` v)

myAppend :: String -> String -> String
myAppend = max

test = P.toList $ foldValues max (each items)
  where items = [ ("a", "asd"), ("a", "def"), ("a", "afg"),
                  ("b", "qwe"), ("b", "fgh"), ("b", "qwer"), ("b", "asd")
                ]

