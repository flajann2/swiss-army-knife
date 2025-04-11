-- | Module    : Utils
-- Description : Various useful functions.
-- Copyright   : (c) 2025 Fred Mitchell & Atomlogik
-- License     : MIT
-- Maintainer  : fred.mitchell@atomlogik.de
-- Stability   : stable
-- Portability : portable
module Utils where

import System.IO
import System.Process
import System.Directory (getDirectoryContents)
import System.FilePath (dropExtension, takeExtension)

isExclusiveOr :: [Bool] -> Bool
isExclusiveOr bs = (length $ filter id bs) == 1

isExclusiveOrNone :: [Bool] -> Bool
isExclusiveOrNone bs = (length $ filter id bs) `elem` exorno
  where 
   exorno = [0, 1]

systemctl :: [String] -> IO [String]
systemctl parms = systemG $ "sudo" : "systemctl" : parms  
      
systemctl_ :: [String] -> IO ()
systemctl_ parms = do
  _ <- systemG $ "sudo" : "systemctl" : parms
  return ()

systemG :: [String] -> IO [String]
systemG (cmd:parms) = do
  (_, Just hout, _, _) <- createProcess (proc cmd parms) { std_out = CreatePipe }
  out <- hGetContents hout
  return $ lines out
      
