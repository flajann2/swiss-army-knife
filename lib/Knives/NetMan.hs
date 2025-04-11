-- | Module    : Knives.NetMan
-- Description : Knife to manage the NetworkManager.
-- Copyright   : (c) 2025 Fred Mitchell & Atomlogik
-- License     : MIT
-- Maintainer  : fred.mitchell@atomlogik.de
-- Stability   : stable
-- Portability : portable

module Knives.NetMan where

import Utils
import CommandLine
import Control.Monad (when)

knifeNetMan :: NetManOptions -> IO ()
knifeNetMan NetManOptions { activateNM
                          , deactivateNM
                          , reactivateNM}
  | isExclusiveOr [ activateNM
                  , deactivateNM
                  , reactivateNM] = do
      _ <- when activateNM   activate
      _ <- when deactivateNM deactivate
      _ <- when reactivateNM reactivate
      return ()
  | otherwise = putStrLn("You must specify one and only one option for NetMan.")
  where
    service = "NetworkManager.service"
    activate   = systemctl_ ["start",   service]
    deactivate = systemctl_ ["stop",    service]
    reactivate = systemctl_ ["restart", service]
