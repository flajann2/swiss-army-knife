{-# LANGUAGE OverloadedStrings #-}

module Knives.Version where

import CommandLine
import qualified Paths_swiss_army_knife as SAK
import Data.Version (showVersion)

knifeVersion :: VersionOptions -> IO ()
knifeVersion _ = do
  putStrLn $ "Swiss Army Knife version: " ++ showVersion SAK.version
  return ()
