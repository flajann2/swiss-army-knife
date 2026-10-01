{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-|
Module      : Knives.Gitit.Yaml
Description : Create remote for local git repo
Maintainer  : fred.mitchell@atomlogik.de

The data amd Yaml support.
-}

module Knives.Gitit.Yaml where

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
import Data.Text (Text)
import GHC.Generics (Generic)
import Control.Monad (when)
import Utils ()
import Control.Monad.Extra (whenJust)
import System.IO (hPutStrLn, stderr)


-- | What kind of Git host a target points to. Determines how
-- knifeGitit talks to it (bare-repo creation locally vs. API/URL
-- handling for a forge).
data TargetKind
  = Local
  | GitHub
  | GitLab
  | Bitbucket
  deriving (Show, Eq, Generic)

instance FromJSON TargetKind where
  parseJSON = withText "TargetKind" $ \t -> case t of
    "local"     -> pure Local
    "github"    -> pure GitHub
    "gitlab"    -> pure GitLab
    "bitbucket" -> pure Bitbucket
    other       -> fail ("Unknown target kind: " <> show other)

instance ToJSON TargetKind where
  toJSON Local     = "local"
  toJSON GitHub    = "github"
  toJSON GitLab    = "gitlab"
  toJSON Bitbucket = "bitbucket"

-- | A single push destination: a name the user picks (mirroring
-- Git's own "origin"/"upstream" convention), what kind of host it
-- is, and either a local path or a remote URL.
data Target = Target
  { name :: String     -- ^ this should be remote, as in name of the remote
  , kind :: TargetKind -- ^ name of the target, local, github, etc.
  , path :: String     -- ^ if kind is Local, this is a directory path. kind is anything else, this is a URL
  } deriving (Show, Generic)

instance FromJSON Target where
  parseJSON = withObject "Target" $ \v -> Target
    <$> v .:  "name"
    <*> v .:  "kind"
    <*> v .: "path"

instance ToJSON Target where
  toJSON t = object
    [ "name" .= name t
    , "kind" .= kind t
    , "path" .= path t
    ]

data GititConfig = GititConfig
  { targets       :: [Target]
  , defaultTarget :: Maybe String
  } deriving (Show, Generic)

instance FromJSON GititConfig where
  parseJSON = withObject "GititConfig" $ \v ->
    GititConfig
      <$> v .:? "targets"       .!= targets defaultGititConfig
      <*> v .:? "defaultTarget" .!= defaultTarget defaultGititConfig

instance ToJSON GititConfig where
  toJSON cfg = object
    [ "targets"       .= targets cfg
    , "defaultTarget" .= defaultTarget cfg
    ]

defaultGititConfig :: GititConfig
defaultGititConfig = GititConfig
  { targets = [ Target { name = "origin"
                        , kind = Local
                        , path = "/repo"
                        }
              ]
  , defaultTarget = Nothing
  }
