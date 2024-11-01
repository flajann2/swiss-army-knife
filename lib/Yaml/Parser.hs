{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Yaml.Parser where

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
import CommandLine
import Options.Applicative.Types (ParserM, OptVisibility)

data ParmType = PTInt 
              | PTFloat 
              | PTString 
  deriving (Show, Generic)

instance FromJSON ParmType where
  parseJSON (String "Int")    = pure PTInt
  parseJSON (String "Float")  = pure PTFloat
  parseJSON (String "String") = pure PTString
  parseJSON _                 = fail "Invalid parameter type"

data OpType = OTSwitch
            | OTOptional
            | OTRequired
  deriving (Show, Generic)

instance FromJSON OpType where
  parseJSON (String "switch")   = pure OTSwitch
  parseJSON (String "optional") = pure OTOptional
  parseJSON (String "required") = pure OTRequired
  parseJSON _                   = fail "Invalid option type"
        
data Option = Option
  { optype :: !OpType
  , long   :: !String
  , short  :: !Char
  , meta   :: !String
  , ptype  :: !ParmType
  , help   :: !String
  } deriving (Show, Generic)

instance FromJSON Option where
  parseJSON = withObject "Option" $ \v -> Option
        <$> v .: "optype"
        <*> v .: "long"
        <*> v .: "short"
        <*> v .: "meta"
        <*> v .: "ptype"
        <*> v .: "help"

data When = When { op_name :: !String
                 , macro   :: ![String]
                 } deriving (Show, Generic)

data Otherwise = Otherwise { macro :: ![String] } deriving (Show, Generic)

data Action = AWhen !When
            | AOtherwise !Otherwise
            | Exe { macro          :: !(Maybe String)
                  , when_cond      :: ![When]
                  , otherwise_cond :: !(Maybe Otherwise)
                  } deriving (Show, Generic)

data Knife = Knife
  { command :: !String    -- subcommand name, lowercased from the Yaml
  , option  :: !(Vector Option)
  , action  :: !Action 
  } deriving (Show, Generic)

instance FromJSON Knife where
    parseJSON = withObject "Knife" $ \v -> Knife 
        <$> v .: "command"
        <*> v .: "option"
        <*> v .: "action"

data Author = Author
  { name  :: !String
  , email :: !String
  } deriving (Show, Generic)

data Macros = Macros
  { name        :: !String
  , description :: !String
  , author      :: !Author
  , copyright   :: !String
  , knives      :: !(Vector Knife)
  } deriving (Show, Generic)


instance FromJSON Author where
    parseJSON = withObject "Author" $ \v -> Author 
        <$> v .: "name"
        <*> v .: "email"

instance FromJSON Macros where
    parseJSON = withObject "Macros" $ \v -> Macros 
        <$> v .: "name"
        <*> v .: "desription"
        <*> v .: "author"
        <*> v .: "copyright"
        <*> v .: "knives"
