-- main for Swiss Army Knife, cli.

module Main where

import Options.Applicative
import Data.Semigroup ((<>))

import CommandLine
import Knives

main :: IO ()
main = do
  (globalOpts, cmd) <- execParser $ info (opts <**> helper)
    ( fullDesc
      <> progDesc "Many useful utilities, such as getting your external IP address, installed kernel, etc."
      <> header "Swiss Army Knife -- Many useful functions for the hacker in all of us." )
  
  case cmd of
    ExtIP extipOpts     -> knifeExtIP extipOpts
    Kernel kernelOpts   -> knifeKernel kernelOpts
    Sleep sleepOpts     -> knifeSleep sleepOpts 
    ZfsCheck zfsOpts    -> knifeZfsCheck zfsOpts
    YamlMacros yamlOpts -> knifeYamlMacros yamlOpts
