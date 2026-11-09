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

import Data.Aeson.Types (Parser, parseMaybe)
import Control.Applicative ((<|>))
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
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
                        , createDirectory
                        , makeAbsolute)
import Data.List (find)
import Data.Char (isSpace, toLower)
import Data.Maybe (fromMaybe)
-- import Config ( getKnifeConfig, setKnifeConfig )
-- import CommandLine ( GititCommand(..) )
import Data.Aeson
    ( withObject
    , object
    , (.:)
    , (.:?)
    , Value(..)
    , decode
    , encode
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

-- | Pull a human-readable error out of a forge's JSON error body.
--   Bitbucket: {"error": {"message": ...}}
--   GitHub:    {"message": ..., "errors": [...]}
--   GitLab:    {"message": ...}  (message may be a string, object or list)
apiMessage :: Value -> Maybe String
apiMessage = parseMaybe $ withObject "error body" $ \o -> bitbucket o <|> plain o
  where
    bitbucket o = (o .: "error") >>= withObject "error" (.: "message")
    plain o = do
      m    <- o .:  "message" :: Parser Value
      errs <- o .:? "errors"  :: Parser (Maybe Value)
      pure $ render m <> maybe "" (\e -> " " <> render e) errs
    render (String t) = T.unpack t
    render v          = BL.unpack (encode v)

-- | Explain an HTTP failure; falls back to a status-based hint when the
-- body is empty (Bitbucket sends a bare 401 for bad credentials).
describeFailure :: Int -> BL.ByteString -> String
describeFailure code body = case decode body >>= apiMessage of
  Just m  -> m
  Nothing
    | not (BL.null body) -> BL.unpack body
    | code == 401        -> "authentication failed (check the token and, for Bitbucket, the email)"
    | code == 403        -> "permission denied (the token lacks the needed scopes or rights)"
    | otherwise          -> "empty response"

-- | Send an API request without ever throwing: network errors, non-2xx
-- statuses and unparseable bodies are all reported on stderr.
apiCall :: String -> Request -> IO (Maybe Value)
apiCall site req = do
  r <- try (httpLBS req) :: IO (Either HttpException (Response BL.ByteString))
  case r of
    Left e -> Nothing <$ hPutStrLn stderr ("gitit: " <> site <> " request failed: " <> show e)
    Right resp -> do
      let code = getResponseStatusCode resp
          body = getResponseBody resp
      if code >= 200 && code < 300
        then case decode body of
          Just v  -> pure (Just v)
          Nothing -> Nothing <$ hPutStrLn stderr
            ("gitit: " <> site <> " returned HTTP " <> show code <> " with a non-JSON body")
        else Nothing <$ hPutStrLn stderr
          ("gitit: " <> site <> " API error (HTTP " <> show code <> "): " <> describeFailure code body)

-- | Return the extracted URL, or report the API's response on stderr.
extractOr :: String -> (Value -> Maybe String) -> Value -> IO (Maybe String)
extractOr site f v = case f v of
  Just u  -> pure (Just u)
  Nothing -> Nothing <$ hPutStrLn stderr ("gitit: " <> site <> " API error: " <> BL.unpack (encode v))

-- | Bitbucket returns clone URLs as links.clone = [{name, href}, ...]
bitbucketSshUrl :: Value -> Maybe String
bitbucketSshUrl = parseMaybe $ withObject "repo" $ \o -> do
  clones <- (o .: "links") >>= (.: "clone")
  pairs  <- mapM (withObject "clone" (\c -> (,) <$> c .: "name" <*> c .: "href")) clones
  maybe (fail "no ssh clone link") pure (lookup ("ssh" :: String) pairs)

-- Local repo / remote handling ---------------------------------------------

-- | Which front-end manages the repo in the current directory.
-- jj is preferred when present, since it also covers colocated repos
-- (where it writes the remote through to .git/config).
data LocalVcs = Jj | Git
  deriving (Show, Eq)

-- | Run a command, never throwing: a missing executable shows up as
-- exit code 127 with the exception text on stderr.
runQuiet :: String -> [String] -> IO (ExitCode, String, String)
runQuiet cmd args = do
  r <- try (readCreateProcessWithExitCode (proc cmd args) "")
         :: IO (Either SomeException (ExitCode, String, String))
  pure $ either (\e -> (ExitFailure 127, "", show e)) id r

-- | Find the VCS for the current directory, if we are inside a repo at all.
detectVcs :: IO (Maybe LocalVcs)
detectVcs = do
  (jc, _, _) <- runQuiet "jj" ["root"]
  if jc == ExitSuccess
    then pure (Just Jj)
    else do
      (gc, out, _) <- runQuiet "git" ["rev-parse", "--is-inside-work-tree"]
      pure $ if gc == ExitSuccess && trim out == "true" then Just Git else Nothing

-- | Does the local repo already have a remote with this name?
remoteExists :: LocalVcs -> String -> IO Bool
remoteExists Git n = (\(c, _, _) -> c == ExitSuccess) <$> runQuiet "git" ["remote", "get-url", n]
remoteExists Jj  n = do
  (c, out, _) <- runQuiet "jj" ["git", "remote", "list"]
  pure $ c == ExitSuccess && any ((== [n]) . take 1 . words) (lines out)

-- | The command that registers a remote, for running and for printing.
remoteAddCmd :: LocalVcs -> String -> String -> (String, [String])
remoteAddCmd Git n url = ("git", ["remote", "add", n, url])
remoteAddCmd Jj  n url = ("jj",  ["git", "remote", "add", n, url])

showCmd :: (String, [String]) -> String
showCmd (c, xs) = unwords (c : xs)

-- | Check, before anything is created remotely, that the remote can be
-- added afterwards. Returns the VCS to use, or reports why not.
preflightRemote :: String -> IO (Maybe LocalVcs)
preflightRemote n = do
  mvcs <- detectVcs
  case mvcs of
    Nothing -> Nothing <$ hPutStrLn stderr
      "gitit: --add-remote given, but the current directory is not inside a git or jj repo"
    Just vcs -> do
      taken <- remoteExists vcs n
      if taken
        then Nothing <$ hPutStrLn stderr
          ("gitit: the local repo already has a remote named " <> n
           <> " (choose another with --remote-name NAME)")
        else pure (Just vcs)

-- | Register the newly-created repo as a remote of the local one.
addLocalRemote :: LocalVcs -> String -> String -> IO ()
addLocalRemote vcs n url = do
  let c@(cmd, args) = remoteAddCmd vcs n url
  (code, _out, err) <- runQuiet cmd args
  case code of
    ExitSuccess   -> putStrLn $ "  Added remote " <> n <> " -> " <> url
    ExitFailure k -> do
      hPutStrLn stderr $ "gitit: adding the remote failed (" <> show k <> "): " <> trim err
      hPutStrLn stderr $ "  The repo was created; add it by hand with:  " <> showCmd c

-- | "sak gitit create TARGET REPONAME [-r | -n NAME]" - create a repo in the
-- specified target, optionally registering it as a remote of the current repo.
createRepo :: GititConfig -> String -> String -> Maybe String -> IO ()
createRepo cfg t r mRemote = case mRemote of
  Nothing -> create Nothing
  Just n  -> preflightRemote n >>= maybe (putStrLn "failed") (\vcs -> create (Just (vcs, n)))
  where
    create mAdd = do
      result <- createRepo' cfg t r
      case result of
        Nothing -> putStrLn "failed"
        Just url -> do
          putStrLn   ""
          putStrLn $ "  Creation of " <> url <> " worked."
          case mAdd of
            Just (vcs, n) -> addLocalRemote vcs n url
            Nothing -> do
              putStrLn   "  To add this remote to your local repo, execute:"
              putStrLn $ "     git remote add " <> t <> " " <> url
              putStrLn   "  or rerun with --add-remote (-r) to have gitit do it."


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
        -- absolute, so the URL still works as a remote from any directory
        repo <- makeAbsolute (t.path <> "/" <> n <> ".git")
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
            maybe (pure Nothing) (extractOr "GitHub" (parseMaybe (withObject "repo" (.: "ssh_url")))) res

      createGitHub' :: B.ByteString -> String -> Bool -> IO (Maybe Value)
      createGitHub' token name priv = do
        let req = setRequestMethod "POST"
                  $ setRequestHeader "Authorization" ["Bearer " <> token]
                  $ setRequestHeader "Accept" ["application/vnd.github+json"]
                  $ setRequestHeader "X-GitHub-Api-Version" ["2022-11-28"]
                  $ setRequestHeader "User-Agent" ["swiss-army-knife-gitit"]  -- GitHub rejects requests without one
                  $ setRequestBodyJSON (object [ "name"    .= name
                                               , "private" .= priv ])
                  $ "https://api.github.com/user/repos"
        apiCall "GitHub" req

      -- GitLab ---------------------------------------------------------------

      createGitLab _t n = do
        host <- gitlabHost
        mtok <- gitlabToken host
        case mtok of
          Nothing -> Nothing <$ hPutStrLn stderr
            "gitit: no GitLab token (set GITLAB_TOKEN or run `glab auth login`)"
          Just tok -> do
            res <- createGitLab' host tok n False
            maybe (pure Nothing) (extractOr "GitLab" (parseMaybe (withObject "project" (.: "ssh_url_to_repo")))) res

      createGitLab' :: String -> B.ByteString -> String -> Bool -> IO (Maybe Value)
      createGitLab' host token name priv = do
        let vis = if priv then "private" else "public" :: String
            req = setRequestHeader "PRIVATE-TOKEN" [token]
                  $ setRequestHeader "User-Agent" ["swiss-army-knife-gitit"]
                  $ setRequestBodyJSON (object [ "name"       .= name
                                               , "path"       .= name
                                               , "visibility" .= vis ])
                  $ parseRequest_ ("POST https://" <> host <> "/api/v4/projects")
        apiCall "GitLab" req

      -- Bitbucket ------------------------------------------------------------

      createBitbucket _t n = do
        mws  <- nonEmptyEnv "BITBUCKET_WORKSPACE"
        mem  <- nonEmptyEnv "BITBUCKET_EMAIL"
        mtok <- nonEmptyEnv "BITBUCKET_TOKEN"
        mprj <- nonEmptyEnv "BITBUCKET_PROJECT"   -- optional project key
        case (,,) <$> mws <*> mem <*> mtok of
          Nothing -> Nothing <$ hPutStrLn stderr
            "gitit: Bitbucket needs BITBUCKET_WORKSPACE, BITBUCKET_EMAIL and BITBUCKET_TOKEN"
          Just (ws, em, tok) -> do
            res <- createBitbucket' ws em tok mprj n False
            maybe (pure Nothing) (extractOr "Bitbucket" bitbucketSshUrl) res

      -- Every Bitbucket repo belongs to a project. Without one, Bitbucket
      -- uses the workspace's default project, which may not allow creation.
      createBitbucket' :: String -> String -> String -> Maybe String -> String -> Bool -> IO (Maybe Value)
      createBitbucket' ws email token mProject name priv = do
        let slug = map toLower name   -- Bitbucket slugs are lowercase
            req  = setRequestBasicAuth (B.pack email) (B.pack token)
                   $ setRequestHeader "User-Agent" ["swiss-army-knife-gitit"]
                   $ setRequestBodyJSON (object ( [ "scm"        .= ("git" :: String)
                                                  , "is_private" .= priv ]
                                                <> maybe [] (\k -> [ "project" .= object [ "key" .= k ] ]) mProject ))
                   $ parseRequest_ ( "POST https://api.bitbucket.org/2.0/repositories/"
                                   <> ws <> "/" <> slug )
        apiCall "Bitbucket" req
