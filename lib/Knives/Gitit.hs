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
import Utils
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
  { name :: String
  , kind :: TargetKind
  , path :: Maybe String   -- ^ used when kind == Local
  , url  :: Maybe String   -- ^ used when kind is a forge
  } deriving (Show, Generic)

instance FromJSON Target where
  parseJSON = withObject "Target" $ \v -> Target
    <$> v .:  "name"
    <*> v .:  "kind"
    <*> v .:? "path"
    <*> v .:? "url"

instance ToJSON Target where
  toJSON t = object
    [ "name" .= name t
    , "kind" .= kind t
    , "path" .= path t
    , "url"  .= url t
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
                        , path = Just "/repo"
                        , url  = Nothing
                        }
              ]
  , defaultTarget = Nothing
  }

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
    listRemoteG = undefined
    addRemoteG  = undefined
    createRepoG = undefined

parseKind :: String -> Maybe TargetKind
parseKind "local"     = Just Local
parseKind "github"    = Just GitHub
parseKind "gitlab"    = Just GitLab
parseKind "bitbucket" = Just Bitbucket
parseKind _           = Nothing

-- | "sak gitit add NAME KIND LOCATION" — register a new target.
-- Rejects a duplicate name and an unrecognized kind rather than
-- silently overwriting or storing garbage.
addTarget :: GititConfig -> String -> String -> String -> IO ()
addTarget cfg nameArg kindStr location = case parseKind kindStr of
  Nothing -> hPutStrLn stderr ("Unknown kind: " <> kindStr)
  Just k
    | any ((== nameArg) . name) (targets cfg) ->
        hPutStrLn stderr ("A target named " <> nameArg <> " already exists")
    | otherwise -> do
        let newTarget = case k of
              Local -> Target { name = nameArg, kind = k
                               , path = Just location, url = Nothing }
              _     -> Target { name = nameArg, kind = k
                               , path = Nothing, url = Just location }
            cfg' = cfg { targets = targets cfg <> [newTarget] }
        setKnifeConfig "gitit" cfg'
        putStrLn ("Added target " <> nameArg)

-- | "sak gitit delete NAME" — remove a target by name. Also clears
-- defaultTarget if it pointed at the one being deleted, so the
-- config never references a target that no longer exists.
deleteTarget :: GititConfig -> String -> IO ()
deleteTarget cfg nameArg
  | not (any ((== nameArg) . name) (targets cfg)) =
      hPutStrLn stderr ("No target named " <> nameArg)
  | otherwise = do
      let remaining = filter ((/= nameArg) . name) (targets cfg)
          newDefault
            | defaultTarget cfg == Just nameArg = Nothing
            | otherwise                          = defaultTarget cfg
          cfg' = cfg { targets = remaining, defaultTarget = newDefault }
      setKnifeConfig "gitit" cfg'
      putStrLn ("Deleted target " <> nameArg)

-- | "sak gitit default NAME" — set which target other commands use
-- when no explicit target/kind is given. Requires the name to
-- already exist, so defaultTarget can never point at nothing.
setDefaultTarget :: GititConfig -> String -> IO ()
setDefaultTarget cfg nameArg
  | not (any ((== nameArg) . name) (targets cfg)) =
      hPutStrLn stderr ("No target named " <> nameArg <> "; add it first")
  | otherwise = do
      let cfg' = cfg { defaultTarget = Just nameArg }
      setKnifeConfig "gitit" cfg'
      putStrLn ("Default target set to " <> nameArg)
