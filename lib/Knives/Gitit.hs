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

import Config ( getKnifeConfig, setKnifeConfig )
import System.Process ()
import CommandLine ( GititCommand(..) )
import Data.Aeson
    ( FromJSON(..)
    , ToJSON(..)
    , withObject
    , withText
    , object
    , (.:)
    , (.:?)
    , (.!=)
    , (.=)
    )
import Knives.Gitit.Yaml
import Knives.Gitit.Target
import Data.Text (Text)
import GHC.Generics (Generic)
import Control.Monad (when)
import Utils ()
import Control.Monad.Extra (whenJust)
import System.IO (hPutStrLn, stderr)
import Data.List (unlines)

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
