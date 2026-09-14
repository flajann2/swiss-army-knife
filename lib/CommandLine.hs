{-# LANGUAGE OverloadedStrings #-}

module CommandLine where

import Options.Applicative
    ( optional,
      auto,
      argument,
      command,
      help,
      info,
      long,
      metavar,
      option,
      progDesc,
      short,
      str,
      strOption,
      subparser,
      switch,
      Parser )

-- Define data types for our commands and options
data Command
  = ExtIP     ExtIPOptions
  | Version   VersionOptions
  | Kernel    KernelOptions
  | Sleep     SleepOptions
  | ZfsCheck  ZfsCheckOptions
  | WireGuard WireGuardOptions
  | NetMan    NetManOptions
  | SysNet    SysNetOptions
  | Gitit     GititCommand
  deriving Show 

data ExtIPOptions = ExtIPOptions
  { nolocation :: Bool
  , ipv6       :: Bool
  } deriving Show

data KernelOptions = KernelOptions
  { justVersion :: Bool } deriving Show

data SleepOptions = SleepOptions
  { secondsToSleep :: Maybe Int } deriving Show

data VersionOptions = VersionOptions Bool deriving Show

data ZfsCheckOptions = ZfsCheckOptions
  { full :: Bool } deriving Show

data WireGuardOptions = WireGuardOptions
  { listWGs      :: Bool
  , activateWG   :: Maybe String
  , enableWG     :: Maybe String
  , deactivateWG :: Bool
  , disableWG    :: Bool
  , reactivateWG :: Bool
  } deriving Show

data NetManOptions = NetManOptions
  { activateNM   :: Bool
  , enableNM     :: Bool
  , deactivateNM :: Bool
  , disableNM    :: Bool
  , reactivateNM :: Bool
  } deriving Show

data SysNetOptions = SysNetOptions
  { activateSN   :: Bool
  , enableSN     :: Bool
  , deactivateSN :: Bool
  , disableSN    :: Bool
  , reactivateSN :: Bool
  } deriving Show

-- gitit is now a sub-command group rather than a flag bag
data GititCommand
  = GititList
  | GititAdd     String
  | GititDelete  String
  | GititDefault String
  | GititCreate  String
  deriving Show

data GlobalOptions = GlobalOptions
  { verbose :: Bool } deriving Show

globalOptionsParser :: Parser GlobalOptions
globalOptionsParser = GlobalOptions
  <$> switch ( long     "verbose"
               <> short 'v'
               <> help  "Enable verbose mode" )

extipOptionsParser :: Parser ExtIPOptions
extipOptionsParser = ExtIPOptions
  <$> switch ( long "nolocation"
               <> short 'n'
               <> help "Don't display location information" )
  <*> switch ( long "ipv6"
               <> short 'i'
               <> help "Display the IPv6 address" )

kernelOptionsParser :: Parser KernelOptions
kernelOptionsParser = KernelOptions
  <$> switch ( long "justversion"
               <> short 'j'
               <> help "Just display the numerical version number, no adjoining text." )

-- version has no options
versionOptionsParser :: Parser VersionOptions
versionOptionsParser = VersionOptions
  <$> switch ( long "noop" )
  
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

wgOptionsParser :: Parser WireGuardOptions
wgOptionsParser = WireGuardOptions
  <$> switch   ( long "list"
                 <> short 'l'
                 <> help "List all installed WireGuardG VPNs")
  <*> optional (strOption ( long "activate"
                              <> short 'a'
                              <> metavar "WGVPN"
                              <> help "WireGuard VPN to activate"))
  <*> optional (strOption ( long "enable"
                              <> short 'A'
                              <> metavar "WGVPN"
                              <> help "WireGuard VPN to enable and activate"))
  <*> switch   ( long "deactivate"
                 <> short 'd'
                 <> help "Deactivate (but not disable) all WireGuard VPNs")
  <*> switch   ( long "disable"
                 <> short 'D'
                 <> help "Disable (but not deactivate) all WireGuard VPNs")
  <*> switch   ( long "reactivate"
                 <> short 'r'
                 <> help "Reactivate currently active WireGuardG VPNs")

nmOptionsParser :: Parser NetManOptions
nmOptionsParser = NetManOptions
  <$> switch   ( long "activate"
                 <> short 'a'
                 <> help "Activate (start) NetworkManager")
  <*> switch   ( long "enable"
                 <> short 'A'
                 <> help "Enable and Activate (start) NetworkManager")
  <*> switch   ( long "deactivate"
                 <> short 'd'
                 <> help "Deactivate (stop) NetworkManager")
  <*> switch   ( long "disable"
                 <> short 'D'
                 <> help "Disable and Deactivate (stop) NetworkManager")
  <*> switch   ( long "reactivate"
                 <> short 'r'
                 <> help "Reactivate (restart) NetworkManager")

snOptionsParser :: Parser SysNetOptions
snOptionsParser = SysNetOptions
  <$> switch   ( long "activate"
                 <> short 'a'
                 <> help "Activate (start) systemd-networkd socket and service")
  <*> switch   ( long "enable"
                 <> short 'A'
                 <> help "Enable and Activate (start) systemd-networkd socket and service")
  <*> switch   ( long "deactivate"
                 <> short 'd'
                 <> help "Deactivate (stop) systemd-networkd socket and service")
  <*> switch   ( long "disable"
                 <> short 'D'
                 <> help "Disable and Deactivate (stop) systemd-networkd socket and service")
  <*> switch   ( long "reactivate"
                 <> short 'r'
                 <> help "Reactivate (restart) systemd-networkd socket and service")

-- Individual gitit sub-command parsers

gititListParser :: Parser GititCommand
gititListParser = pure GititList

gititAddParser :: Parser GititCommand
gititAddParser = GititAdd
  <$> argument str (metavar "REMOTE" <> help "Remote repo to add")

gititDeleteParser :: Parser GititCommand
gititDeleteParser = GititDelete
  <$> argument str (metavar "REMOTE" <> help "Remote repo to delete")

gititDefaultParser :: Parser GititCommand
gititDefaultParser = GititDefault
  <$> argument str (metavar "REMOTE" <> help "Remote repo to make the default")

gititCreateParser :: Parser GititCommand
gititCreateParser = GititCreate
  <$> argument str (metavar "REMOTE" <> help "Remote repo to create a new repository in")

-- Combine gitit's sub-commands into one parser
gititOptionsParser :: Parser GititCommand
gititOptionsParser = subparser
  (    command "list"    (info gititListParser    (progDesc "List known remote repositories"))
    <> command "add"     (info gititAddParser     (progDesc "Add a remote repo"))
    <> command "delete"  (info gititDeleteParser  (progDesc "Delete a remote repo"))
    <> command "default" (info gititDefaultParser (progDesc "Make repo the default"))
    <> command "create"  (info gititCreateParser  (progDesc "Create a new repo in the remote repository"))
  )

-- Combine the subcommand parsers
commandParser :: Parser Command
commandParser = subparser
  (    command "extip"    (info (ExtIP     <$> extipOptionsParser)    (progDesc "Display external IP address"))
    <> command "kernel"   (info (Kernel    <$> kernelOptionsParser)   (progDesc "Display kernel information, both installed and currently running"))
    <> command "sleep"    (info (Sleep     <$> sleepOptionsParser)    (progDesc "Put the machine to sleep"))
    <> command "version"  (info (Version   <$> versionOptionsParser)  (progDesc "Version of Swiss Army Knife"))
    <> command "zfscheck" (info (ZfsCheck  <$> zfscheckOptionsParser) (progDesc "Check zfs availibility for the current kernel"))
    <> command "wg"       (info (WireGuard <$> wgOptionsParser)       (progDesc "Manage WireGuard VPNs"))
    <> command "nm"       (info (NetMan    <$> nmOptionsParser)       (progDesc "Manage NetworkManager"))
    <> command "sn"       (info (SysNet    <$> snOptionsParser)       (progDesc "Manage systemd-networkd"))
    <> command "gitit"    (info (Gitit     <$> gititOptionsParser)    (progDesc "Manage gitit remote repositories"))
  )

-- Combine global options with the command parser
opts :: Parser (GlobalOptions, Command)
opts = (,) <$> globalOptionsParser <*> commandParser
