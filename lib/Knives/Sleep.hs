{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Knives.Sleep
-- Description : Knife to put the OS to sleep.
-- Copyright   : (c) 2025 Fred Mitchell & Atomlogik
-- License     : MIT
-- Maintainer  : fred.mitchell@atomlogik.de
-- Stability   : stable
-- Portability : portable

module Knives.Sleep where

import System.Process
import CommandLine

knifeSleep :: SleepOptions -> IO ()
knifeSleep optsS = do
  putStrLn $ "Put the machine to sleep." ++ show (secondsToSleep optsS)
  case (secondsToSleep optsS) of
    Just secs -> do putStrLn $ "sleep in " ++ show secs ++ " seconds."
                    _n <- readProcess "sleep" [show secs] ""
                    return ()
    Nothing  ->  putStrLn "sleep immediatly"
  _nn <-  readProcess "systemctl" ["suspend", "-i"] ""
  return ()
