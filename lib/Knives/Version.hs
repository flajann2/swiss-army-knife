{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Knives.Version
-- Description : Knife to print the sak version.
-- Copyright   : (c) 2025 Fred Mitchell & Atomlogik
-- License     : MIT
-- Maintainer  : fred.mitchell@atomlogik.de
-- Stability   : stable
-- Portability : portable

module Knives.Version where

import CommandLine
import qualified Paths_swiss_army_knife_hs as SAK
import Data.Version (showVersion)

knifeVersion :: VersionOptions -> IO ()
knifeVersion _ = do
  putStrLn $ "Swiss Army Knife version: " ++ showVersion SAK.version
  return ()
