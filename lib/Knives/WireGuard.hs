{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Knives.WireGuard
-- Description : Knife to manage WireGuard activations.
-- Copyright   : (c) 2025 Fred Mitchell & Atomlogik
-- License     : MIT
-- Maintainer  : fred.mitchell@atomlogik.de
-- Stability   : stable
-- Portability : portable

module Knives.WireGuard where

import Utils (systemctl)
import CommandLine
import System.IO
import System.Process
import System.Directory (getDirectoryContents)
import System.FilePath (dropExtension, takeExtension)
import Control.Monad (when)
import Data.List

knifeWireGuard :: WireGuardOptions -> IO ()
knifeWireGuard WireGuardOptions { listWGs
                                , activateWG
                                , deactivateWG
                                , reactivateWG}
  | checkMutualExclusivity = do
      wgs <- wgList
      ss  <- wgStatus wgs
      when listWGs $ putStrLn $ formatStatus ss
      case (activateWG, deactivateWG, reactivateWG) of
        (Just vpn, False, False) -> activate vpn
        (Nothing, True, False)   -> deactivate
        (Nothing, False, True)   -> reactivate
        _                        -> return ()
  | otherwise = putStrLn "Options aside from --list or -l are mutually exclusive"
  where
    wgDir = "/etc/wireguard/"
    wgList :: IO [String]
    wgList = do
      rawlist <- getDirectoryContents wgDir
      let wgconf = filter (\f -> takeExtension f == ".conf") rawlist
      let wgs = map (\f -> dropExtension f) wgconf
      return wgs
      
    checkMutualExclusivity = check [ activateWG /= Nothing
                                   , deactivateWG
                                   , reactivateWG]

    check bs   = (length $ filter id bs) `elem` [0, 1]
    vpn2wg vpn = "wg-quick@" ++ vpn
    
    activate :: String -> IO ()
    activate vpn = do
      _ <- systemctl ["start", vpn2wg vpn]
      return ()

    deactivate :: IO ()
    deactivate   = do
      awgs <- activeWG
      mapM_ (\w -> systemctl $ ["stop"] ++ [vpn2wg w]) awgs
      return ()

    reactivate :: IO ()
    reactivate   = do
      awgs <- activeWG
      mapM_ (\w -> systemctl $ ["restart"] ++  [vpn2wg w]) awgs
      return()

    wgStatus :: [String] -> IO [(String, String, Bool)]
    wgStatus wgs = do
      let quick = map (\w -> vpn2wg w) wgs
      let cmd = ["is-active"] ++ quick
      stats <- systemctl cmd
      return $ [(wg, sact, sact == "active") | (wg, sact) <- zip wgs stats  ]

    formatStatus :: [(String, String, Bool)] -> String
    formatStatus wss = intercalate "\n" [w ++ ": " ++ s |(w, s, _) <- wss]

    activeWG :: IO [String]
    activeWG = do
      wgl <- wgList
      wgs <- wgStatus wgl
      return [s | (s, _, b) <- wgs, b]
