-- | Config.hs — load or create the Swiss Army Knife configuration file,
-- exposed as a lazily-initialized, cached global via IORef.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Config
  ( Config(..)
  , getConfig
  , configDir
  , configFile
  ) where

import Data.IORef
import Data.Yaml
import GHC.Generics (Generic)
import System.Directory
    ( XdgDirectory(XdgConfig)
    , getXdgDirectory
    , createDirectoryIfMissing
    , doesFileExist
    )
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)

-- | Application configuration, loaded from ~/.config/swiss-army-knife/config.yaml
data Config = Config
  { extIpService   :: String
  , wireguardIface :: String
  , zfsPools       :: [String]
  } deriving (Show, Generic)

instance FromJSON Config
instance ToJSON Config

defaultConfig :: Config
defaultConfig = Config
  { extIpService   = "https://ifconfig.me"
  , wireguardIface = "wg0"
  , zfsPools       = ["zpool"]
  }

appName :: String
appName = "swiss-army-knife"

configDir :: IO FilePath
configDir = getXdgDirectory XdgConfig appName

configFile :: IO FilePath
configFile = (</> "config.yaml") <$> configDir

-- | Global cache, empty until the first knife that needs config asks for it.
{-# NOINLINE configCache #-}
configCache :: IORef (Maybe Config)
configCache = unsafePerformIO (newIORef Nothing)

-- | Load the config, creating the directory and a default file if missing.
loadConfig :: IO Config
loadConfig = do
  dir  <- configDir
  file <- configFile
  createDirectoryIfMissing True dir

  exists <- doesFileExist file
  if not exists
    then do
      encodeFile file defaultConfig
      pure defaultConfig
    else do
      result <- decodeFileEither file
      case result of
        Right cfg -> pure cfg
        Left err  -> do
          hPutStrLn stderr $
            "Warning: failed to parse " <> file <> ": "
              <> prettyPrintParseException err
          hPutStrLn stderr "Falling back to default configuration."
          pure defaultConfig

-- | Get the config, loading and caching it on first use.
-- Knives that don't need config never trigger the disk read.
getConfig :: IO Config
getConfig = do
  cached <- readIORef configCache
  case cached of
    Just cfg -> pure cfg
    Nothing  -> do
      cfg <- loadConfig
      writeIORef configCache (Just cfg)
      pure cfg
