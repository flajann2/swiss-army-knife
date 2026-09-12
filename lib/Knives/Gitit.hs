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
import Config ( getKnifeConfig )
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data GititConfig = GititConfig
  { remotes :: [String]
  } deriving (Show, Generic)

instance FromJSON GititConfig
instance ToJSON GititConfig

defaultGititConfig :: GititConfig
defaultGititConfig = GititConfig { remotes = ["/repo"] }

knifeGitit :: GititOptions -> IO ()
knifeGitit opts = do
  cfg <- getKnifeConfig "gitit" defaultGititConfig
  pure ()
