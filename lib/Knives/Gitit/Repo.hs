{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE MultiWayIf #-}

{-|
Module      : Knives.Gitit.Repo
Description : Repo creation and manipulation
Maintainer  : fred.mitchell@atomlogik.de

Deals with repo maitenance for the various
collections like Local, GitHub, Gitlab, etc.
-}

module Knives.Gitit.Repo where

import Data.Aeson.Types (parseMaybe)
import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as B
import System.Process (CreateProcess(..)
                      , proc
                      , readCreateProcessWithExitCode
                      , readProcess)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.Directory ( doesFileExist
                        , doesDirectoryExist
                        , createDirectory)
import Data.List (find)
import Data.Char (isSpace)
-- import Config ( getKnifeConfig, setKnifeConfig )
import System.Process ()
-- import CommandLine ( GititCommand(..) )
import Data.Aeson
    ( withObject
    , object
    , (.:)
    , Value
    , KeyValue((.=)) )
import Control.Exception (try, SomeException)
-- import Control.Monad (when)
import Utils ()
-- import Control.Monad.Extra (whenJust)
import System.IO (hPutStrLn, stderr)
import Knives.Gitit.Yaml

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

trim :: [Char] -> [Char]
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- | fetches the GITHUB_TOKEN environment variable, or derives it from gh
githubToken :: IO (Maybe B.ByteString)
githubToken = do
  env <- lookupEnv "GITHUB_TOKEN"
  case env of
    Just t | not (null t) -> pure (Just (B.pack t))
    _ -> do
      r <- try (readProcess "gh" ["auth", "token"] "") :: IO (Either SomeException String)
      pure $ case r of
        Right out | let t = trim out, not (null t) -> Just (B.pack t)
        _ -> Nothing
        
-- | "sak gitit create REMOTENAME REPONAME" - create a repo in the specified target
createRepo :: GititConfig -> String -> String -> IO ()
createRepo cfg t r = do
  result <- createRepo' cfg t r
  case result of
    Just r'  -> do
      putStrLn   ""
      putStrLn $ "  Creation of " <> r' <> " worked."
      putStrLn $ "  To add this remote to your local repo, execute:"
      putStrLn $ "     git remote add origin " <> r'
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

      createGitHub _t n = do
        mtok <- githubToken
        case mtok of
          Nothing -> do
            putStrLn "gitit: no GitHub token (set GITHUB_TOKEN or run `gh auth login`)"
            pure Nothing
          Just tok -> do
            res <- createGitHub' tok n False
            pure $ parseMaybe (withObject "repo" (.: "ssh_url")) res
        
      createGitHub' :: B.ByteString -> String -> Bool -> IO Value
      createGitHub' token name priv = do
        let req = setRequestMethod "POST"
                  $ setRequestHeader "Authorization" ["Bearer " <> token]
                  $ setRequestHeader "Accept" ["application/vnd.github+json"]
                  $ setRequestHeader "X-GitHub-Api-Version" ["2022-11-28"]
                  $ setRequestHeader "User-Agent" ["swiss-army-knife-gitit"]  -- GitHub rejects requests without one
                  $ setRequestBodyJSON (object ["name" .= name
                                               , "private" .= priv])
                  $ "https://api.github.com/user/repos"
        getResponseBody <$> httpJSON req

      createGitLab    = undefined
      createBitbucket = undefined
      
