{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Config ( getKnifeConfig )
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data PlaygroundConfig = PlaygroundConfig
  { list     :: [String]
  , name     :: Maybe String
  , switchit :: Bool
  } deriving (Show, Generic)

instance FromJSON PlaygroundConfig
instance ToJSON PlaygroundConfig

defaultPlaygroundConfig :: PlaygroundConfig
defaultPlaygroundConfig = PlaygroundConfig { list = ["one", "two", "three"]
                                           , switchit = False
                                           }

main :: IO ()
main = do
  cfg <- getKnifeConfig "playground" defaultPlaygroundConfig
  putStrLn $ show cfg
  pure ()
