module Knives.SysNet where

import Utils
import CommandLine
import Control.Monad (when)

knifeSysNet :: SysNetOptions -> IO ()
knifeSysNet SysNetOptions { activateSN
                          , enableSN
                          , deactivateSN
                          , disableSN
                          , reactivateSN }
  | isExclusiveOr [ activateSN
                  , enableSN
                  , deactivateSN
                  , disableSN
                  , reactivateSN] = do
      when activateSN   activate
      when enableSN     enable
      when deactivateSN deactivate
      when disableSN    disable
      when reactivateSN reactivate
  | otherwise = putStrLn("You must specify one and only one option for SysNet.")
  where
    socket  = "systemd-networkd.socket"
    service = "systemd-networkd.service"
    
    activate   = do
      systemctl_ ["start",   socket]
      systemctl_ ["start",   service]

    enable     = do
      systemctl_ ["start",   socket]
      systemctl_ ["enable", "--now",  service]

    deactivate = do
      systemctl_ ["stop",   socket]
      systemctl_ ["stop",   service]
 
    disable    = do
      systemctl_ ["stop",   socket]
      systemctl_ ["disable", "--now", service]
 
    reactivate = do
      systemctl_ ["restart",   socket]
      systemctl_ ["restart",   service]
