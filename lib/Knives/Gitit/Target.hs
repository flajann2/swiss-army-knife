{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE MultiWayIf #-}

{-|
Module      : Knives.Gitit.Target
Description : Create remote for local git repo
Maintainer  : fred.mitchell@atomlogik.de

The data amd Yaml support.
-}

module Knives.Gitit.Target where

import Network.HTTP.Simple ()
import System.Directory ()
import Config ( setKnifeConfig )
import System.Process ()
import Data.Aeson ()
import Utils ()
import System.IO (hPutStrLn, stderr)
import Knives.Gitit.Yaml

parseKind :: String -> Maybe TargetKind
parseKind "local"     = Just Local
parseKind "github"    = Just GitHub
parseKind "gitlab"    = Just GitLab
parseKind "bitbucket" = Just Bitbucket
parseKind _           = Nothing

-- | "sak gitit add KIND REMOTE LOCATION" — register a new target.
-- Rejects a duplicate name and an unrecognized kind rather than
-- silently overwriting or storing garbage.
addTarget :: GititConfig -> String -> String -> String -> IO ()
addTarget cfg kind remote location = case parseKind kind of
  Nothing -> hPutStrLn stderr ("Unknown kind: " <> kind)
  Just k
    | any ((== remote) . name) (targets cfg) ->
        hPutStrLn stderr ("A target named " <> remote <> " already exists")
    | otherwise -> do
        let newTarget = Target { name = remote
                               , kind = k
                               , path = location }
            cfg' = cfg { targets = targets cfg <> [newTarget] }
        setKnifeConfig "gitit" cfg'
        putStrLn ("Added target " <> remote)

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


