-- | main for Swiss Army Knife, cli.

module Main where

import Options.Applicative
    ( (<**>), fullDesc, header, info, progDesc, execParser, helper )
import CommandLine
    ( Command(SysNet, ExtIP, Kernel, Sleep, Version, ZfsCheck,
              WireGuard, NetMan),
      opts )

import Knives.ExtIP ( knifeExtIP )
import Knives.Kernel ( knifeKernel )
import Knives.NetMan ( knifeNetMan )
import Knives.Sleep ( knifeSleep )
import Knives.SysNet ( knifeSysNet )
import Knives.Version ( knifeVersion )
import Knives.WireGuard ( knifeWireGuard )
import Knives.ZfsCheck ( knifeZfsCheck )

main :: IO ()
main = do
  (_, cmd) <- execParser $ info (opts <**> helper)
    ( fullDesc
      <> progDesc "Many useful utilities, such as getting your external IP address, installed kernel, controlling your WireGuard, etc."
      <> header "Swiss Army Knife -- Many useful functions for the hacker in all of us." )

  case cmd of
    ExtIP extipOpts     -> knifeExtIP     extipOpts
    Kernel kernelOpts   -> knifeKernel    kernelOpts
    Sleep sleepOpts     -> knifeSleep     sleepOpts
    Version versionOpts -> knifeVersion   versionOpts
    ZfsCheck zfsOpts    -> knifeZfsCheck  zfsOpts
    WireGuard wgOpts    -> knifeWireGuard wgOpts
    NetMan nmOpts       -> knifeNetMan    nmOpts
    SysNet snOpts       -> knifeSysNet    snOpts
