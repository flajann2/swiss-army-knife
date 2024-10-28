module Knives.ZfsCheck where

import Data.List (isInfixOf)
import Data.List.Split (splitOn)
import Data.Char (isSpace)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import System.Process
import CommandLine

knifeZfsCheck :: ZfsCheckOptions -> IO ()
knifeZfsCheck opts = do
  putStrLn "Check the version of archzfs with repo kernel version so you can determine if an upgrade is possible."

  let url_zfs = "https://raw.githubusercontent.com/openzfs/zfs/master/META"

  repo_version  <- readProcess "pacman" ["-Si", "linux"] "" -- | awk '/^Version/ { print $3 }'" ""
  archzfs_version <- readProcess "curl" ["-sSL", url_zfs] ""
  skernel <- readProcess "uname" ["-r"] ""
  sinstalled <- readProcess "pacman" ["-Q", "linux"] ""

  let repo    = splitAndTrim
                $ LBS8.unpack
                $ head [v | v <- LBS8.lines (LBS8.pack repo_version), LBS8.pack "Version" `LBS.isPrefixOf` v]
      archzfs = splitAndTrim
                $ LBS8.unpack
                $ head [v | v <- LBS8.lines (LBS8.pack archzfs_version), LBS8.pack "Linux-Maximum:" `LBS.isPrefixOf` v]
      running = trim skernel
      installed = unwords
                $ filter (not . ("linux" `isInfixOf`))
                $ words
                $ trim sinstalled

  putStrLn $ "\n      repo: " ++ repo
    ++ "\n       zfs: " ++ archzfs
    ++ "\n   running: " ++ running
    ++ "\n installed: " ++ installed

  where
    splitAndTrim line = trim $ last $ splitOn ":" line
    trim = f . f
      where f = reverse . dropWhile isSpace
