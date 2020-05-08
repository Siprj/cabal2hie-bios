{-# LANGUAGE OverloadedStrings #-}

module Stat
    ( dirStats
    , fileStats
    )
  where

import Data.List
import Data.Map
import Data.Text
import Text.Pretty.Simple

import Lib


dirStats :: FilePath -> IO [(FilePath, (Int, [Text]))]
dirStats cabalFile = do
    kwa <- parseCabalFileSimple cabalFile
    let kwa2 = fmap (\(name, _, dirs)-> (name, dirs)) kwa
    let kwa3 = Prelude.concat $ fmap (\(name, dirs) -> fmap (\dir -> (name, dir)) dirs) kwa2
    let kwa4 = Prelude.foldl (\m (name, dir) -> insertWith (\(c1, name1) (c2, name2) -> (c1 + c2, name1 <> name2)) dir (1, [name]) m) Data.Map.empty  kwa3
    let kwa5 = sortBy (\a b -> fst (snd b) `compare` fst (snd a)) $ Data.Map.toList kwa4
    pure kwa5


fileStats :: FilePath -> IO [(FilePath, (Int, [Text]))]
fileStats cabalFile = do
    kwa <- parseCabalFileSimple cabalFile
    let kwa2 = fmap (\(name, files, _)-> (name, files)) kwa
    let kwa3 = Prelude.concat $ fmap (\(name, dirs) -> fmap (\dir -> (name, dir)) dirs) kwa2
    let kwa4 = Prelude.foldl (\m (name, dir) -> insertWith (\(c1, name1) (c2, name2) -> (c1 + c2, name1 <> name2)) dir (1, [name]) m) Data.Map.empty  kwa3
    let kwa5 = sortBy (\a b -> fst (snd b) `compare` fst (snd a)) $ Data.Map.toList kwa4
    pure kwa5
