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
  case cmd of
    GititList             -> listRemoteG cfg
    GititAdd nameArg      -> addRemoteG cfg nameArg
    GititDelete nameArg   -> deleteTarget cfg nameArg
    GititDefault nameArg  -> setDefaultTarget cfg nameArg
    GititCreate nameArg   -> createRepoG cfg nameArg
  where
    listRemoteG cfg   = putStrLn $ unlines [t.name <> " - " <> show t.kind | t <- cfg.targets]
    addRemoteG  cfg a = undefined
    createRepoG cfg a = undefined

-- >>> getKnifeConfig "gitit" defaultGititConfig 
-- GititConfig {targets = [Target {name = "github", kind = GitHub, path = Just "https://github.com", url = Nothing},Target {name = "origin", kind = Local, path = Just "/repo", url = Nothing}], defaultTarget = Nothing}
