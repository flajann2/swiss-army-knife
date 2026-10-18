{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Data.Char (isSpace, toLower)
import Data.Maybe (fromMaybe)
-- import Config ( getKnifeConfig, setKnifeConfig )
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

-- | An environment variable, treating "" as unset.
nonEmptyEnv :: String -> IO (Maybe String)
nonEmptyEnv v = do
  e <- lookupEnv v
  pure $ case e of
    Just s | not (null s) -> Just s
    _                     -> Nothing

-- | Token from an env var, falling back to a CLI command's output.
tokenFrom :: String -> String -> [String] -> IO (Maybe B.ByteString)
tokenFrom var cmd args = do
  env <- nonEmptyEnv var
  case env of
    Just t  -> pure (Just (B.pack t))
    Nothing -> do
      r <- try (readProcess cmd args "") :: IO (Either SomeException String)
      pure $ case r of
        Right out | let t = trim out, not (null t) -> Just (B.pack t)
        _ -> Nothing

-- | fetches the GITHUB_TOKEN environment variable, or derives it from gh
githubToken :: IO (Maybe B.ByteString)
githubToken = tokenFrom "GITHUB_TOKEN" "gh" ["auth", "token"]

-- | GitLab host; honours GITLAB_HOST (same variable glab uses) for self-hosted instances.
gitlabHost :: IO String
gitlabHost = fromMaybe "gitlab.com" <$> nonEmptyEnv "GITLAB_HOST"

-- | fetches GITLAB_TOKEN, or derives it from glab's config
gitlabToken :: String -> IO (Maybe B.ByteString)
gitlabToken host = tokenFrom "GITLAB_TOKEN" "glab" ["config", "get", "token", "--host", host]

-- | Return the extracted URL, or report the API's response on stderr.
extractOr :: String -> (Value -> Maybe String) -> Value -> IO (Maybe String)
extractOr site f v = case f v of
  Just u  -> pure (Just u)
  Nothing -> Nothing <$ hPutStrLn stderr ("gitit: " <> site <> " API error: " <> show v)

-- | Bitbucket returns clone URLs as links.clone = [{name, href}, ...]
bitbucketSshUrl :: Value -> Maybe String
bitbucketSshUrl = parseMaybe $ withObject "repo" $ \o -> do
  clones <- (o .: "links") >>= (.: "clone")
  pairs  <- mapM (withObject "clone" (\c -> (,) <$> c .: "name" <*> c .: "href")) clones
  maybe (fail "no ssh clone link") pure (lookup ("ssh" :: String) pairs)

-- | "sak gitit create REMOTENAME REPONAME" - create a repo in the specified target
createRepo :: GititConfig -> String -> String -> IO ()
createRepo cfg t r = do
  result <- createRepo' cfg t r
  case result of
    Just r'  -> do
      putStrLn   ""
      putStrLn $ "  Creation of " <> r' <> " worked."
      putStrLn   "  To add this remote to your local repo, execute:"
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

      -- Local ----------------------------------------------------------------

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

      -- GitHub ---------------------------------------------------------------

      createGitHub _t n = do
        mtok <- githubToken
        case mtok of
          Nothing -> Nothing <$ hPutStrLn stderr
            "gitit: no GitHub token (set GITHUB_TOKEN or run `gh auth login`)"
          Just tok -> do
            res <- createGitHub' tok n False
            extractOr "GitHub" (parseMaybe (withObject "repo" (.: "ssh_url"))) res

      createGitHub' :: B.ByteString -> String -> Bool -> IO Value
      createGitHub' token name priv = do
        let req = setRequestMethod "POST"
                  $ setRequestHeader "Authorization" ["Bearer " <> token]
                  $ setRequestHeader "Accept" ["application/vnd.github+json"]
                  $ setRequestHeader "X-GitHub-Api-Version" ["2022-11-28"]
                  $ setRequestHeader "User-Agent" ["swiss-army-knife-gitit"]  -- GitHub rejects requests without one
                  $ setRequestBodyJSON (object [ "name"    .= name
                                               , "private" .= priv ])
                  $ "https://api.github.com/user/repos"
        getResponseBody <$> httpJSON req

      -- GitLab ---------------------------------------------------------------

      createGitLab _t n = do
        host <- gitlabHost
        mtok <- gitlabToken host
        case mtok of
          Nothing -> Nothing <$ hPutStrLn stderr
            "gitit: no GitLab token (set GITLAB_TOKEN or run `glab auth login`)"
          Just tok -> do
            res <- createGitLab' host tok n False
            extractOr "GitLab" (parseMaybe (withObject "project" (.: "ssh_url_to_repo"))) res

      createGitLab' :: String -> B.ByteString -> String -> Bool -> IO Value
      createGitLab' host token name priv = do
        let vis = if priv then "private" else "public" :: String
            req = setRequestHeader "PRIVATE-TOKEN" [token]
                  $ setRequestHeader "User-Agent" ["swiss-army-knife-gitit"]
                  $ setRequestBodyJSON (object [ "name"       .= name
                                               , "path"       .= name
                                               , "visibility" .= vis ])
                  $ parseRequest_ ("POST https://" <> host <> "/api/v4/projects")
        getResponseBody <$> httpJSON req

      -- Bitbucket ------------------------------------------------------------

      createBitbucket _t n = do
        mws  <- nonEmptyEnv "BITBUCKET_WORKSPACE"
        mem  <- nonEmptyEnv "BITBUCKET_EMAIL"
        mtok <- nonEmptyEnv "BITBUCKET_TOKEN"
        case (,,) <$> mws <*> mem <*> mtok of
          Nothing -> Nothing <$ hPutStrLn stderr
            "gitit: Bitbucket needs BITBUCKET_WORKSPACE, BITBUCKET_EMAIL and BITBUCKET_TOKEN"
          Just (ws, em, tok) -> do
            res <- createBitbucket' ws em tok n False
            extractOr "Bitbucket" bitbucketSshUrl res

      createBitbucket' :: String -> String -> String -> String -> Bool -> IO Value
      createBitbucket' ws email token name priv = do
        let slug = map toLower name   -- Bitbucket slugs are lowercase
            req  = setRequestBasicAuth (B.pack email) (B.pack token)
                   $ setRequestHeader "User-Agent" ["swiss-army-knife-gitit"]
                   $ setRequestBodyJSON (object [ "scm"        .= ("git" :: String)
                                                , "is_private" .= priv ])
                   $ parseRequest_ ( "POST https://api.bitbucket.org/2.0/repositories/"
                                   <> ws <> "/" <> slug )
        getResponseBody <$> httpJSON req
