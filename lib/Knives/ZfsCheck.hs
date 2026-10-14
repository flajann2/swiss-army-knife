module Knives.ZfsCheck
  ( knifeZfsCheck
  ) where

import CommandLine (ZfsCheckOptions(..))
import Control.Applicative ((<|>))
import Control.Exception (IOException, try)
import Data.Char (isDigit, isSpace)
import Data.List (dropWhileEnd, stripPrefix)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import System.Directory (doesFileExist)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

-- | Decide whether the kernel in the Arch repo can be installed without
-- losing ZFS.
--
-- The kernel ceiling has to come from the OpenZFS release that archzfs
-- would actually install, not from OpenZFS master: master's META always
-- advertises the newest kernel the developers are targeting, which can be
-- far ahead of what any packaged release supports.
knifeZfsCheck :: ZfsCheckOptions -> IO ()
knifeZfsCheck opts = do
  putStrLn "Check whether archzfs supports the repo kernel, so you can determine if an upgrade is possible."

  repoKernel <- (>>= field "Version") <$> pkgInfo "linux"
  running    <- maybe "?" trim <$> run "uname" ["-r"]
  installed  <- maybe "?" (unwords . drop 1 . words) <$> run "pacman" ["-Q", "linux"]

  -- The ZFS release the repos would give you (DKMS route first, then utils).
  dkmsInfo  <- pkgInfo "zfs-dkms"
  utilsInfo <- pkgInfo "zfs-utils"
  let zfsVer = takeWhile (/= '-') <$> ((dkmsInfo <|> utilsInfo) >>= field "Version")

  meta <- maybe (pure Nothing) zfsMeta zfsVer
  let linuxMax = meta >>= field "Linux-Maximum" . snd

  -- Prebuilt modules are pinned to one exact kernel build.
  prebuiltKernel <- (>>= linuxDep) <$> pkgInfo "zfs-linux"

  let dkmsOk     = (<=) <$> (repoKernel >>= majorMinor) <*> (linuxMax >>= majorMinor)
      prebuiltOk = (==) <$> repoKernel <*> prebuiltKernel

  putStrLn $ "\n      repo kernel: " ++ orQ repoKernel
    ++ "\n          running: " ++ running
    ++ "\n        installed: " ++ installed
    ++ "\n  zfs (repo/dkms): " ++ orQ zfsVer
    ++ maybe "" (\m -> "  (supports Linux up to " ++ m ++ ")") linuxMax
    ++ "\nzfs-linux targets: " ++ fromMaybe "(not available)" prebuiltKernel

  putStrLn $ "\n  DKMS route:     " ++ verdict dkmsOk
    ++ "\n  Prebuilt route: " ++ verdict prebuiltOk

  case (dkmsOk, prebuiltOk) of
    (Just True, _) -> putStrLn "\nUpgrade possible: install linux, linux-headers and the ZFS packages in one transaction."
    (_, Just True) -> putStrLn "\nUpgrade possible via zfs-linux (exact kernel match)."
    _              -> putStrLn "\nDo NOT upgrade the kernel: no available ZFS package supports it yet."

  if full opts
    then putStrLn $ "\nKernel ceiling read from: " ++ maybe "(none)" fst meta
    else pure ()

  where
    orQ = fromMaybe "?"
    verdict (Just True)  = "OK"
    verdict (Just False) = "NO"
    verdict Nothing      = "unknown (missing data)"

-- | @pacman -Si pkg@, or Nothing if the package isn't in any repo.
pkgInfo :: String -> IO (Maybe String)
pkgInfo pkg = run "pacman" ["-Si", pkg]

-- | META of the given OpenZFS release, with where it came from.
-- Uses the locally installed DKMS source when it matches, otherwise the
-- release tag on GitHub.
zfsMeta :: String -> IO (Maybe (String, String))
zfsMeta ver = do
  let local = "/usr/src/zfs-" ++ ver ++ "/META"
      url   = "https://raw.githubusercontent.com/openzfs/zfs/zfs-" ++ ver ++ "/META"
  haveLocal <- doesFileExist local
  if haveLocal
    then Just . (,) local <$> readFile local
    else fmap ((,) url) <$> run "curl" ["-fsSL", url]

-- | Run a command; Nothing on a non-zero exit or if it can't be started.
run :: String -> [String] -> IO (Maybe String)
run cmd args = do
  r <- try @IOException (readProcessWithExitCode cmd args "")
  pure $ case r of
    Right (ExitSuccess, out, _) -> Just out
    _                           -> Nothing

-- | Value of a @Name : value@ field (pacman -Si and META style),
-- joining pacman's indented continuation lines.
field :: String -> String -> Maybe String
field name = go . lines
  where
    go [] = Nothing
    go (l:ls) = case stripPrefix name l of
      Just rest
        | (pre, ':':v) <- break (== ':') rest
        , all isSpace pre ->
            let cont = takeWhile (any isSpace . take 1) ls
            in Just (trim (unwords (map trim (v : cont))))
      _ -> go ls

-- | The @linux=VERSION@ constraint from a package's dependencies.
linuxDep :: String -> Maybe String
linuxDep info = field "Depends On" info
  >>= listToMaybe . mapMaybe (stripPrefix "linux=") . words

-- | Major and minor version: "7.2.11.arch1-1" -> (7, 2), "6.15" -> (6, 15).
majorMinor :: String -> Maybe (Int, Int)
majorMinor v = case splitOn "." (takeWhile (\c -> isDigit c || c == '.') v) of
  (a:b:_) | not (null a), not (null b) -> Just (read a, read b)
  _                                    -> Nothing

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
