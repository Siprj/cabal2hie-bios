{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Semigroup
import Data.Text.IO
import Data.Text
import System.Environment

import Lib


main :: IO ()
main = do
    args <- getArgs
    components <- parseCabalFile $ args !! 0
    Data.Text.IO.writeFile "hie.yaml" $ bla components
  where
    magic :: [Text]
    magic = ["cradle:", "  cabal:"]
    bla :: [BiosCabalComponent] -> Text
    bla bios = Data.Text.unlines $ magic <> bla2 bios
    bla2 :: [BiosCabalComponent] -> [Text]
    bla2 bios = (fmap componentToHieFile bios)
