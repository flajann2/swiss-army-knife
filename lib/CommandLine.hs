-- Command Line Parser 
{-# LANGUAGE OverloadedStrings #-}

module CommandLine where

import Options.Applicative
    ( optional
    , auto
    , command
    , help
    , info
    , long
    , metavar
    , option
    , progDesc
    , short
    , str
    , subparser
    , switch
    , Parser )
-- import Data.Semigroup ((<>))

-- Define data types for our commands and options
data Command
  = ExtIP      !ExtIPOptions
  | Kernel     !KernelOptions
  | Sleep      !SleepOptions
  | ZfsCheck   !ZfsCheckOptions
  | YamlMacros !YamlMacrosOptions
  deriving Show 

data ExtIPOptions = ExtIPOptions
  { nolocation :: !Bool
  , ipv6 :: !Bool
  } deriving Show

data KernelOptions = KernelOptions
  { justVersion :: !Bool } deriving Show

data SleepOptions = SleepOptions
  { secondsToSleep :: Maybe Int } deriving Show

data ZfsCheckOptions = ZfsCheckOptions
  { full :: !Bool } deriving Show

data YamlMacrosOptions = YamlMacrosOptions
  { file :: Maybe String
  , save :: !Bool
  } deriving Show

data GlobalOptions = GlobalOptions
  { verbose :: !Bool } deriving Show

globalOptionsParser :: Parser GlobalOptions
globalOptionsParser = GlobalOptions
  <$> switch
      ( long "verbose"
     <> short 'v'
     <> help "Enable verbose mode" )

extipOptionsParser :: Parser ExtIPOptions
extipOptionsParser = ExtIPOptions
  <$> switch
  ( long "nolocation"
    <> short 'n'
    <> help "Don't display location information" )
  <*> switch
  ( long "ipv6"
    <> short 'i'
    <> help "Display the IPv6 address" )

kernelOptionsParser :: Parser KernelOptions
kernelOptionsParser = KernelOptions
  <$> switch
  ( long "justversion"
    <> short 'j'
    <> help "Just display the numerical version number, no adjoining text." )

sleepOptionsParser :: Parser SleepOptions
sleepOptionsParser = SleepOptions
  <$> optional (option auto ( long "wait"
                              <> short 'w'
                              <> metavar "SECONDS"
                              <> help "Seconds to wait before going to sleep"))

zfscheckOptionsParser :: Parser ZfsCheckOptions
zfscheckOptionsParser = ZfsCheckOptions
  <$> switch
  ( long "full"
    <> short 'f'
    <> help "Check all kernel versions" )

yamlOptionsParser :: Parser YamlMacrosOptions
yamlOptionsParser = YamlMacrosOptions
  <$> optional (option str  (long "file"
                            <> short 'f'
                            <> metavar "PATHNAME"
                            <> help "Maco file to load"))
  <*> switch (long "save"
             <> short 's'
             <> help "save macro permanently")
  
-- Combine the subcommand parsers
commandParser :: Parser Command
commandParser = subparser
  (    command "extip"    (info (ExtIP      <$> extipOptionsParser) (progDesc "Display external IP address"))
    <> command "kernel"   (info (Kernel     <$> kernelOptionsParser) (progDesc "Display kernel information, both installed and currently running"))
    <> command "sleep"    (info (Sleep      <$> sleepOptionsParser) (progDesc "Put the machine to sleep"))
    <> command "zfscheck" (info (ZfsCheck   <$> zfscheckOptionsParser) (progDesc "Check ZFS kernel versions for compatibility"))
    <> command "macros"   (info (YamlMacros <$> yamlOptionsParser) (progDesc "Load Yaml Macros"))
  )

-- Combine global options with the command parser
opts :: Parser (GlobalOptions, Command)
opts = (,) <$> globalOptionsParser <*> commandParser
