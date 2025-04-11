-- | Module    : Knives.SysNet
-- Description : Knife to manage systemd-networkd.
-- Copyright   : (c) 2025 Fred Mitchell & Atomlogik
-- License     : MIT
-- Maintainer  : fred.mitchell@atomlogik.de
-- Stability   : stable
-- Portability : portable

module Knives.SysNet where

import Utils
import CommandLine
import Control.Monad (when)

knifeSysNet :: SysNetOptions -> IO ()
knifeSysNet SysNetOptions { activateSN
                          , deactivateSN
                          , reactivateSN}
  | isExclusiveOr [ activateSN
                  , deactivateSN
                  , reactivateSN] = do
      when activateSN   activate
      when deactivateSN deactivate
      when reactivateSN reactivate
  | otherwise = putStrLn("You must specify one and only one option for SysNet.")
  where
    socket  = "systemd-networkd.socket"
    service = "systemd-networkd.service"
    
    activate   = do
      systemctl_ ["start",   socket]
      systemctl_ ["start",   service]

    deactivate = do
      systemctl_ ["stop",   socket]
      systemctl_ ["stop",   service]
 
    reactivate = do
      systemctl_ ["restart",   socket]
      systemctl_ ["restart",   service]
