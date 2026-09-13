-- | Config.hs — generic per-knife configuration storage.
-- The on-disk YAML file is a flat map from knife name to that knife's
-- own config blob. Knives own their own types; nobody needs to know
-- about anyone else's schema, and a knife that doesn't call
-- getKnifeConfig never touches the file at all.

{-# LANGUAGE OverloadedStrings #-}

module Config
  ( getKnifeConfig
  , setKnifeConfig
  , configDir
  , configFile
  ) where

import Data.Aeson (FromJSON, ToJSON, Value(..), fromJSON, toJSON, Result(..))
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Key (fromText)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml (decodeFileEither, encodeFile, prettyPrintParseException)
import System.Directory
    ( XdgDirectory(XdgConfig)
    , getXdgDirectory
    , createDirectoryIfMissing
    , doesFileExist
    )
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)

appName :: String
appName = "swiss-army-knife"

configDir :: IO FilePath
configDir = getXdgDirectory XdgConfig appName

configFile :: IO FilePath
configFile = (</> "config.yaml") <$> configDir

-- | Whole config file as a raw JSON object, cached after first load so
-- repeated getKnifeConfig calls in one run don't re-hit disk.
{-# NOINLINE configCache #-}
configCache :: IORef (Maybe Value)
configCache = unsafePerformIO (newIORef Nothing)

loadRawConfig :: IO Value
loadRawConfig = do
  dir  <- configDir
  file <- configFile
  createDirectoryIfMissing True dir
  exists <- doesFileExist file
  if not exists
    then pure (Object KM.empty)
    else do
      result <- decodeFileEither file
      case result of
        Right val -> pure val
        Left err  -> do
          hPutStrLn stderr $
            "Warning: failed to parse " <> file <> ": "
              <> prettyPrintParseException err
          hPutStrLn stderr "Starting from an empty configuration."
          pure (Object KM.empty)

getRawConfig :: IO Value
getRawConfig = do
  cached <- readIORef configCache
  case cached of
    Just v  -> pure v
    Nothing -> do
      v <- loadRawConfig
      writeIORef configCache (Just v)
      pure v

persistRawConfig :: Value -> IO ()
persistRawConfig v = do
  file <- configFile
  encodeFile file v
  writeIORef configCache (Just v)

-- | Get a knife's own config section, identified by a unique key
-- (e.g. "wireguard", "zfscheck"). If the section is missing, or fails
-- to parse as that knife's type, the given default is written into
-- the file under that key — leaving every other knife's section
-- untouched — and returned.
getKnifeConfig :: (FromJSON a, ToJSON a) => Text -> a -> IO a
getKnifeConfig key def = do
  raw <- getRawConfig
  case raw of
    Object obj ->
      case KM.lookup (fromText key) obj of
        Just v -> case fromJSON v of
          Success a -> pure a
          Error err -> do
            hPutStrLn stderr $
              "Warning: config section \"" <> T.unpack key
                <> "\" is invalid: " <> err
            hPutStrLn stderr "Using defaults for this section."
            writeDefault obj
        Nothing -> writeDefault obj
    _ -> writeDefault KM.empty
  where
    writeDefault obj = do
      let obj' = KM.insert (fromText key) (toJSON def) obj
      persistRawConfig (Object obj')
      pure def

-- | Unconditionally write a knife's config section, overwriting
-- whatever was there before. Use this for knife operations that
-- explicitly mutate config (add/delete/set-default), as opposed to
-- getKnifeConfig's "read, or seed with defaults on first use."
setKnifeConfig :: ToJSON a => Text -> a -> IO ()
setKnifeConfig key val = do
  raw <- getRawConfig
  let obj = case raw of
        Object o -> o
        _        -> KM.empty
      obj' = KM.insert (fromText key) (toJSON val) obj
  persistRawConfig (Object obj')
