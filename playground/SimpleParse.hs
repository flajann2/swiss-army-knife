{-# LANGUAGE DeriveFunctor #-}

-- Define the data types for the DSL
data Config
    = AppConfig String [Setting]  -- Application name and its settings
    deriving (Show)

data Setting
    = Database String String      -- Database type and connection string
    | LogLevel String             -- Log level (e.g., "info", "debug")
    | FeatureToggle String Bool    -- Feature name and its toggle state
    deriving (Show)

-- Example of a nested structure
data NestedSetting
    = Nested String [Setting]      -- Nested settings
    deriving (Show)

-- Combinator to create an application configuration
appConfig :: String -> [Setting] -> Config
appConfig name settings = AppConfig name settings

-- Combinator for database settings
database :: String -> String -> Setting
database dbType connectionString = Database dbType connectionString

-- Combinator for log level settings
logLevel :: String -> Setting
logLevel level = LogLevel level

-- Combinator for feature toggles
featureToggle :: String -> Bool -> Setting
featureToggle featureName isEnabled = FeatureToggle featureName isEnabled

-- Combinator for nested settings
nestedSetting :: String -> [Setting] -> NestedSetting
nestedSetting name settings = Nested name settings
