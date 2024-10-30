module Knives.Kernel where

import System.Process
import CommandLine

knifeKernel :: KernelOptions -> IO ()
knifeKernel opt = do
  skernel <- readProcess "uname" ["-r"] ""
  sinstalled <- readProcess "pacman" ["-Q", "linux"] ""
  sinstalled_lts <- readProcess "pacman" ["-Q", "linux-lts"] ""

  if not $ justVersion opt
    then putStrLn $ "      running: " ++ skernel
                 ++ "    installed: " ++ sinstalled 
                 ++ "installed LTS: " ++ sinstalled_lts
    else putStrLn skernel

