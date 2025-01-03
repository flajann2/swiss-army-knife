{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}

module Yaml.Parser where

import Control.Exception
import System.IO
import GHC.Generics (Generic)
import qualified Data.Yaml as Y
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Yaml.Combinators
import Data.YAML.Aeson (decode1)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V

import Network.HTTP.Simple

import Data.Aeson
import Data.Aeson.Schema
import Data.Aeson.Schema.Internal
 
import Data.Bool (bool)
import Data.List (isInfixOf)
import Data.List.Split (splitOn)
import Data.Char (isSpace)
import Data.HashMap.Strict (keys)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Control.Monad ((>=>), unless)
import Control.Applicative (empty)
import System.Process
import CommandLine
import Options.Applicative.Types (ParserM, OptVisibility)

type KnifeSchema = [schema|
{
    name: Text,
    description: Text,
    author: {
        name: Text,
        email: Text
    },
    copyright: {
        holder: Text,
        year: Int
    },
    knives: List {
        knife: {
            command: Text,
            option: List {
                optype: Text,
                long: Text,
                short: Text,
                help: Text
            },
            action: {
                exe: List Text,
                unless: List Text,
                when: List {
                    conditionName: Text,
                    commands: List Text
                },
                otherwise: List Text 
            }
        }
    }
}
|]

-- Define your data types
data KnifeDocument = KnifeDocument
    { name        :: Text
    , description :: Text
    , author      :: Author
    , copyright   :: Copyright
    , knives      :: [Knife]
    } deriving (Show, Generic)

data Author = Author
    { name  :: Text
    , email :: Text
    } deriving (Show, Generic)

data Copyright = Copyright
    { holder :: Text
    , year   :: Int
    } deriving (Show, Generic)

data Knife = Knife
    { command :: Text
    , options :: [Option]
    , action  :: Action
    } deriving (Show, Generic)

data Option = Option
    { optype :: Text
    , long   :: Text
    , short  :: Text
    , help   :: Text
    } deriving (Show, Generic)

data Action = Action
    { exe      :: [Text]
    , unless   :: [Text]
    , when     :: [(Text, [Text])]  -- Condition name and corresponding commands.
    , otherwise:: [Text]
    } deriving (Show, Generic)

-- Make all data types instances of FromJSON for Aeson decoding.
instance FromJSON KnifeDocument where 
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

instance FromJSON Author where 
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

instance FromJSON Copyright where 
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

instance FromJSON Knife where 
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

instance FromJSON Option where 
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

instance FromJSON Action where 
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

--- instance FromJSON KnifeDocument
--- instance FromJSON Author
--- instance FromJSON Copyright
--- instance FromJSON Knife
--- instance FromJSON Option
--- instance FromJSON Action

-- Main function to read and decode the YAML file.
parseYaml :: IO ()
parseYaml = do
  yamlData <- LBS8.readFile "playground/sampleknife.yaml" 
  case Y.decodeEither' yamlData of 
    Left err -> 
      putStrLn $ "Error parsing YAML: " ++ show err 
    Right yamlValue -> do
      let jsonByteString = encode yamlValue :: BS.ByteString 
      case eitherDecodeWithSchema (toSchemaDef KnifeDocument :: Schema KnifeDocument) jsonByteString of
        Left err -> 
          putStrLn $ "Error parsing JSON: " ++ show err 
        Right config -> 
          -- Handle successful parsing 
          print config

---    -- Read the YAML file.
---    yamlData <- BS.readFile "playground/sampleknife.yaml"
---    let yamlData' = LBS8.fromStrict yamlData
---    
---    -- Decode the YAML data into a JSON-compatible format.
---    case decode1 yamlData' of
---        Left err    -> putStrLn $ "Error parsing YAML: " ++ show err
---        Right value -> case eitherDecode value of
---            Left msg     -> putStrLn $ "Failed to decode JSON: " ++ msg
---            Right config -> print (config :: KnifeDocument)

--- data ParmType = PTInt 
---               | PTFloat 
---               | PTString 
---   deriving (Show, Generic)
--- 
--- instance FromJSON ParmType where
---   parseJSON (String "Int")    = pure PTInt
---   parseJSON (String "Float")  = pure PTFloat
---   parseJSON (String "String") = pure PTString
---   parseJSON _                 = fail "Invalid parameter type"
--- 
--- data OpType = OTSwitch
---             | OTOptional
---             | OTRequired
---   deriving (Show, Generic)
--- 
--- instance FromJSON OpType where
---   parseJSON (String "switch")   = pure OTSwitch
---   parseJSON (String "optional") = pure OTOptional
---   parseJSON (String "required") = pure OTRequired
---   parseJSON _                   = fail "Invalid option type"
---         
--- data Option = Option
---   { optype :: !OpType
---   , long   :: !String
---   , short  :: !Char
---   , meta   :: !String
---   , ptype  :: !ParmType
---   , help   :: !String
---   } deriving (Show, Generic)
--- 
--- 
--- instance FromJSON Option where
---   parseJSON = withObject "Option" $ \v -> Option
---         <$> v .: "optype"
---         <*> v .: "long"
---         <*> v .: "short"
---         <*> v .: "meta"
---         <*> v .: "ptype"
---         <*> v .: "help"
--- 
--- data BashLine = BashLine { line :: String } deriving (Show, Generic)
--- 
--- data BashBlock = Exe       [BashLine] 
---                | When      { op_name :: String, lines    :: [BashLine] }
---                | Unless    { op_name :: String, lines    :: [BashLine] }
---                | Otherwise { lines :: [BashLine] }
---                deriving (Show, Generic)
--- 
--- data Action = Action { bash :: [BashBlock] } deriving (Show, Generic)
--- 
--- instance FromJSON Action where
---     parseJSON = withObject "Action" $ \v -> Action
---         <$> v .: "codes"
--- 
--- data Knife = Knife
---   { command :: !String    -- subcommand name, lowercased from the Yaml
---   , option  :: !(Vector Option)
---   , action  :: !Action 
---   } deriving (Show, Generic)
--- 
--- instance FromJSON Knife where
---     parseJSON = withObject "Knife" $ \v -> Knife 
---         <$> v .: "command"
---         <*> v .: "option"
---         <*> v .: "action"
--- 
--- data Author = Author
---   { name  :: !String
---   , email :: !String
---   } deriving (Show, Generic)
--- 
--- data Macros = Macros
---   { name        :: !String
---   , description :: !String
---   , author      :: !Author
---   , copyright   :: !String
---   , knives      :: !(Vector Knife)
---   } deriving (Show, Generic)
--- 
--- 
--- instance FromJSON Author where
---     parseJSON = withObject "Author" $ \v -> Author 
---         <$> v .: "name"
---         <*> v .: "email"
--- 
--- instance FromJSON Macros where
---     parseJSON = withObject "Macros" $ \v -> Macros 
---         <$> v .: "name"
---         <*> v .: "desription"
---         <*> v .: "author"
---         <*> v .: "copyright"
---         <*> v .: "knives"
