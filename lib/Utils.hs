{-|
Module      : Utils
Description : Core utiities to interact with Linux
Maintainer  : fred.mitchell@atomlogik.de

For system commands, issue them using sudo as necessary
for priviledge esclation. User account must be set up accordingly
or will be prompted for the password on every invocation.
-}

module Utils where

import System.IO ( hGetContents )
import System.Process
    ( createProcess
    , proc
    , CreateProcess(std_out)
    , StdStream(CreatePipe)
    )

-- | does an exlusive or on the given list of bools.
-- WARN: Cannot handle inflinite lists.
isExclusiveOr :: [Bool] -> Bool
isExclusiveOr bs = (length $ filter id bs) == 1

-- | does an exlusive or on the given list of bools.
-- If none, allow for this.
-- WARN: Cannot handle inflinite lists.
isExclusiveOrNone :: [Bool] -> Bool
isExclusiveOrNone bs = (length $ filter id bs) `elem` exorno
  where 
   exorno = [0, 1]

-- | run systemctl command with parameters
systemctl :: [String] -> IO [String]
systemctl parms = systemG $ "sudo" : "systemctl" : parms  

-- | run systemctl command with parameters and ignore the results      
systemctl_ :: [String] -> IO ()
systemctl_ parms = do
  _ <- systemG $ "sudo" : "systemctl" : parms
  return ()

-- | execute the given list as a command.
systemG :: [String] -> IO [String]
systemG [] = return []
systemG (cmd:parms) = do
  (_, Just hout, _, _) <- createProcess (proc cmd parms) { std_out = CreatePipe }
  out <- hGetContents hout
  return $ lines out
