{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-|
Module      : Gitit
Description : Create remote for local git repo
Maintainer  : fred.mitchell@atomlogik.de

For a locally defined git repo, create the remote version and
move push it there. 
-}
module Knives.Gitit where

import System.Process
import CommandLine
import Config 

knifeGitit :: GititOptions -> IO ()
knifeGitit opts = undefined
  --cfg <- getConfig
  --let iface = wireguardIface cfg
  -- ... use iface, opts, etc.
