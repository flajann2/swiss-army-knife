{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Knives.YamlMacros where

import System.IO
import GHC.Generics (Generic)
import qualified Data.Yaml as Y
import qualified Data.ByteString.Char8 as BS
import Data.Yaml.Combinators
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V

import Network.HTTP.Simple
import Data.Aeson
import Data.Bool (bool)
import Data.List (isInfixOf)
import Data.List.Split (splitOn)
import Data.Char (isSpace)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Control.Monad ((>=>), unless)
import System.Process
import Yaml.Parser
import CommandLine ( YamlMacrosOptions(file) )
import Options.Applicative.Types (ParserM, OptVisibility)


knifeYamlMacros :: YamlMacrosOptions -> IO ()
knifeYamlMacros opt = do
  case file opt of
    Just pn -> do
      yamlfile <- readFile pn
      putStrLn yamlfile
    Nothing -> putStrLn "No Yaml file given."
