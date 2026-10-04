{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-|
Module      : Knives.Gitit
Description : Create remote for local git repo
Maintainer  : fred.mitchell@atomlogik.de

For a locally defined git repo, create the remote version and
move push it there. 
-}

module Knives.Gitit where

import Config ( getKnifeConfig )
import System.Process ()
import CommandLine ( GititCommand(..) )
import Knives.Gitit.Yaml
import Knives.Gitit.Target
import Knives.Gitit.Repo
import Utils ()

-- | Dispatch on the gitit sub-command.
knifeGitit :: GititCommand -> IO ()
knifeGitit cmd = do
  cfg <- getKnifeConfig "gitit" defaultGititConfig
  path <- case cmd of
    GititList                       -> listTarget cfg
    GititAdd target remote location -> addTarget cfg target remote location
    GititDelete name                -> deleteTarget cfg name
    GititDefault name               -> setDefaultTarget cfg name
    GititCreate kind name           -> createRepo cfg kind name
  pure ()
-- >>> getKnifeConfig "gitit" defaultGititConfig 
-- GititConfig {targets = [Target {name = "github", kind = GitHub, path = "https://github.com"},Target {name = "origin", kind = Local, path = "/repo"}], defaultTarget = Nothing}
