module Knives.Kernel
  ( knifeKernel
  , KernelOptions(..)
  ) where

import CommandLine (KernelOptions(..))
import System.Process (readProcess)
import Control.Exception (try, IOException)
import Data.Char (isSpace)
import Data.List (dropWhileEnd, isInfixOf, isPrefixOf)
import System.Directory (doesFileExist)
import Data.Maybe (catMaybes)

-- | Trim leading and trailing whitespace/newlines
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

-- | Safely run a command. Returns Nothing if it fails or command not found.
tryReadProcess :: String -> [String] -> IO (Maybe String)
tryReadProcess cmd args = do
  result <- try @IOException (readProcess cmd args "")
  pure $ either (const Nothing) (Just . trim) result

-- | Detect Debian-based systems (Debian, Ubuntu, Mint, Pop!_OS, Kali, etc.)
isDebianBased :: IO Bool
isDebianBased = do
  hasDebian <- doesFileExist "/etc/debian_version"
  if hasDebian
    then pure True
    else checkOsRelease
  where
    checkOsRelease = do
      hasOs <- doesFileExist "/etc/os-release"
      if not hasOs then pure False else do
        content <- readFile "/etc/os-release"
        let debianLike = ["debian", "ubuntu", "linuxmint", "pop", "kali", "elementary", "zorin"]
        pure $ any (\idName ->
                      ("ID="        ++ idName) `isInfixOf` content ||
                      ("ID_LIKE="   ++ idName) `isInfixOf` content
                   ) debianLike

-- | Detect Red Hat family (RHEL, CentOS, Fedora, Rocky, AlmaLinux, Oracle, etc.)
isRedHatBased :: IO Bool
isRedHatBased = do
  hasRedhat <- doesFileExist "/etc/redhat-release"
  if hasRedhat
    then pure True
    else checkOsRelease
  where
    checkOsRelease = do
      hasOs <- doesFileExist "/etc/os-release"
      if not hasOs then pure False else do
        content <- readFile "/etc/os-release"
        let redhatLike = ["rhel", "centos", "fedora", "rocky", "almalinux", "ol", "scientific"]
        pure $ any (\idName ->
                      ("ID="      ++ idName) `isInfixOf` content ||
                      ("ID_LIKE=" ++ idName) `isInfixOf` content
                   ) redhatLike

-- | Get the currently running kernel (works on all Linux distros)
getRunningKernel :: IO String
getRunningKernel = do
  mVer <- tryReadProcess "uname" ["-r"]
  pure $ maybe "unknown" id mVer

-- | Get installed kernel packages (distro-aware)
getInstalledKernels :: IO [String]
getInstalledKernels = do
  debian <- isDebianBased
  if debian
    then getDebianKernels
    else do
      redhat <- isRedHatBased
      if redhat
        then getRedHatKernels
        else getArchKernels

-- | Debian / Ubuntu / Mint style
getDebianKernels :: IO [String]
getDebianKernels = do
  mOut <- tryReadProcess "dpkg-query"
            ["-W", "-f=${Package} ${Version}\n", "linux-image-*"]
  case mOut of
    Just out | not (null out) ->
      pure $ filter (not . null) $ lines out
    _ -> do
      -- Fallback to dpkg -l
      mOut2 <- tryReadProcess "dpkg" ["-l"]
      case mOut2 of
        Just out2 ->
          pure $ take 8
               $ map (unwords . take 4 . words)
               $ filter ("linux-image" `isInfixOf`) (lines out2)
        Nothing -> pure []

-- | Red Hat family (RHEL, CentOS, Fedora, Rocky, AlmaLinux, etc.)
getRedHatKernels :: IO [String]
getRedHatKernels = do
  mOut <- tryReadProcess "rpm"
            [ "-qa"
            , "--qf"
            , "%{NAME}-%{VERSION}-%{RELEASE}\n"
            , "kernel*"
            ]
  case mOut of
    Just out | not (null (trim out)) ->
      pure $ take 8
           $ filter (\pkg ->
               "kernel" `isPrefixOf` pkg &&
               not ("headers"   `isInfixOf` pkg) &&
               not ("devel"     `isInfixOf` pkg) &&
               not ("doc"       `isInfixOf` pkg) &&
               not ("tools"     `isInfixOf` pkg)
             )
           $ lines (trim out)
    _ -> pure []

-- | Arch Linux style
getArchKernels :: IO [String]
getArchKernels = do
  mLinux <- tryReadProcess "pacman" ["-Q", "linux"]
  mLts   <- tryReadProcess "pacman" ["-Q", "linux-lts"]
  mZen   <- tryReadProcess "pacman" ["-Q", "linux-zen"]
  mHard  <- tryReadProcess "pacman" ["-Q", "linux-hardened"]
  pure $ catMaybes [mLinux, mLts, mZen, mHard]

-- | Main entry point for the `sak kernel` command
knifeKernel :: KernelOptions -> IO ()
knifeKernel optsK = do
  running <- getRunningKernel

  if justVersion optsK
    then putStrLn running
    else do
      installed <- getInstalledKernels

      putStrLn $ "Running kernel:  " ++ running

      if null installed
        then putStrLn "Installed kernels: (could not determine on this distribution)"
        else do
          putStrLn "Installed kernels:"
          mapM_ (putStrLn . ("  " ++)) installed
