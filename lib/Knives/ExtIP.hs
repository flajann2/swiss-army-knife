module Knives.ExtIP where

import Network.HTTP.Simple
import Data.Aeson
import Data.Bool (bool)
import Control.Monad ((>=>), unless)
import CommandLine
import Data.String (fromString)

newtype RExtIP = RExtIP
  { ip :: String } deriving Show

fs :: String -> Key
fs = fromString

instance FromJSON RExtIP where
  parseJSON = withObject "RExtIP" $ \v -> RExtIP
    <$> v .: fs "ip"

data RLoc = RLoc
  {status      :: !String
  , country    :: !String
  , countryCode:: !String
  , region     :: !String
  , regionName :: !String
  , city       :: !String
  , zip        :: !String
  , lat        :: !Float
  , lon        :: !Float
  , timezone   :: !String
  , isp        :: !String
  , org        :: !String
  , as         :: !String
  , query      :: !String
  } deriving Show

instance FromJSON RLoc where
  parseJSON = withObject "RLoc" $ \v -> RLoc
    <$> v .: fs "status"
    <*> v .: fs "country"
    <*> v .: fs "countryCode"
    <*> v .: fs "region"
    <*> v .: fs "regionName"
    <*> v .: fs "city"
    <*> v .: fs "zip"
    <*> v .: fs "lat"
    <*> v .: fs "lon"
    <*> v .: fs "timezone"
    <*> v .: fs "isp"
    <*> v .: fs "org"
    <*> v .: fs "as"
    <*> v .: fs "query"
  
knifeExtIP :: ExtIPOptions -> IO ()
knifeExtIP opt = do
  res <- (parseRequest >=> httpLBS) $ bool url_ipv4 url_ipv6 $ ipv6 opt
  putStrLn $ ipaddr res
  unless (nolocation opt) $ do
      loc <- location
      putStrLn loc
  where
    url_ipv4 = "https://ipv4.jsonip.com"
    url_ipv6 = "https://ipv6.jsonip.com"
    url_geo  = "http://ip-api.com/json/"
    ipaddr res = do
      let ipresult = eitherDecode $ getResponseBody res :: Either String RExtIP
      case ipresult of
        Left err -> do
          "Failed to obtain IP: " ++ err
        Right extIP -> do
          ip extIP

    location :: IO String
    location = do
      request <- parseRequest url_geo
      res <- httpLBS request
      let locres = eitherDecode $ getResponseBody res :: Either String RLoc
      case locres of
        Left err -> do
          return $ "Failed to obtain location: " ++ err
        Right loc -> do
          let str_result = "Location: "
                ++ city loc ++ ", "
                ++ country loc
                ++ " (" ++ countryCode loc ++ ")\n"
                ++ "Timezone: " ++ timezone loc ++ "\n"                
          return str_result
