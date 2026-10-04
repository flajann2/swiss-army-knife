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

import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as B
import System.Process (CreateProcess(..), proc, readCreateProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.Directory (doesPathExist
                        , doesFileExist
                        , doesDirectoryExist
                        , createDirectory
                        , createDirectoryIfMissing)
import Data.List (find)
import Config ( getKnifeConfig, setKnifeConfig )
import System.Process ()
import CommandLine ( GititCommand(..) )
import Data.Aeson
    ( FromJSON(..)
    , ToJSON(..)
    , withObject
    , withText
    , object
    , Value
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


-- | "sak gitit list" -- list all targets
listTarget :: GititConfig -> IO ()
listTarget cfg = putStrLn $ unlines [t.name <> " - " <> show t.kind | t <- cfg.targets]


-- | Look up a target by its name.
findTarget :: GititConfig -> String -> Maybe Target
findTarget cfg n = find (\t -> t.name == n) cfg.targets

data PathKind = IsFile | IsDir | Missing
  deriving (Show, Eq)

pathKind :: FilePath -> IO PathKind
pathKind p = do
  isFile <- doesFileExist p
  isDir  <- doesDirectoryExist p
  pure $ if | isFile    -> IsFile
            | isDir     -> IsDir
            | otherwise -> Missing
            
-- | "sak gitit create REMOTENAME REPONAME" - create a repo in the specified target
createRepo :: GititConfig -> String -> String -> IO ()
createRepo cfg t r = do
  result <- createRepo' cfg t r
  case result of
    Just r  -> do
      putStrLn   ""
      putStrLn $ "  Creation of " <> r <> " worked."
      putStrLn $ "  To add this remote to your local repo, execute:"
      putStrLn $ "     git remote add origin " <> r
      putStrLn   "  you can replace 'origin' with another target name"
    Nothing -> putStrLn "failed"
      

createRepo' :: GititConfig -> String -> String -> IO (Maybe String)
createRepo' cfg targetName repoName =
  case findTarget cfg targetName of
    Nothing -> Nothing <$ hPutStrLn stderr ("No target named " <> targetName)
    Just t  -> case t.kind of
      Local     -> createLocal     t repoName
      GitHub    -> createGitHub    t repoName
      GitLab    -> createGitLab    t repoName
      Bitbucket -> createBitbucket t repoName
    where
      createLocal
        , createGitHub
        , createGitLab
        , createBitbucket :: Target -> String -> IO (Maybe String)
      createLocal t n = do
        let repo = t.path <> "/" <> n <> ".git"
        pk <- pathKind repo
        case pk of
          Missing -> initRepo repo
          IsFile  -> Nothing <$ hPutStrLn stderr (repo <> " already exists and is a file.")
          IsDir   -> Nothing <$ hPutStrLn stderr (repo <> " already exists.")
        
      initRepo repo = do
        putStrLn $ "Creating new repo " <> repo
        createDirectory repo
        (code
          , _out
          , err) <- readCreateProcessWithExitCode ((proc "git" ["init", "--bare"]) { cwd = Just repo }) ""
        case code of
          ExitSuccess   -> do
            putStrLn $ "Initialized bare repo at " <> repo
            return $ Just repo
          ExitFailure n -> do
            hPutStrLn stderr $ "git init failed (" <> show n <> "): " <> err
            return Nothing
    
      createGitHub    = undefined
      createGithub' :: B.ByteString -> String -> Bool -> IO Value
      createGithub' token name priv = do
        let req = setRequestMethod "POST"
                  $ setRequestHeader "Authorization" ["Bearer " <> token]
                  $ setRequestHeader "Accept" ["application/vnd.github+json"]
                  $ setRequestHeader "X-GitHub-Api-Version" ["2022-11-28"]
                  $ setRequestHeader "User-Agent" ["swiss-army-knife-gitit"]  -- GitHub rejects requests without one
                  $ setRequestBodyJSON (object ["name" .= name, "private" .= priv])
                  $ "https://api.github.com/user/repos"
        getResponseBody <$> httpJSON req

      createGitLab    = undefined
      createBitbucket = undefined
      
