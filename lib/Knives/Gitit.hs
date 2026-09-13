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

import System.Process ()
import CommandLine ( GititOptions(..) )
import Config ( getKnifeConfig )
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Control.Monad (when)
import Utils
import Control.Monad.Extra (whenJust)

-- whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()

data GititConfig = GititConfig
  { remotes :: [String]
  } deriving (Show, Generic)

instance FromJSON GititConfig
instance ToJSON GititConfig

defaultGititConfig :: GititConfig
defaultGititConfig = GititConfig { remotes = ["/repo"] }

knifeGitit :: GititOptions -> IO ()
knifeGitit GititOptions { listRemote
                        , addRemote
                        , deleteRemote
                        , defaultRemote
                        , createRepo
                        } = do
  cfg <- getKnifeConfig "gitit" defaultGititConfig
  when     listRemote listRemoteG
  whenJust addRemote addRemoteG
  whenJust deleteRemote deleteRemoteG
  whenJust defaultRemote defaultRemoteG
  whenJust createRepo createRepoG
  pure ()
    where
      listRemoteG = undefined
      addRemoteG = undefined
      deleteRemoteG = undefined
      defaultRemoteG = undefined
      createRepoG = undefined
