module TVM.Opts
    ( Command(..)
    , getOpts
    , commands
    ) where

import Options.Applicative
import Data.Monoid (mconcat)

data Command =
      CmdCreate { createName :: String }
    | CmdList
    | CmdStart { startVNC :: Bool, startConsole :: Bool, startName :: String }
    | CmdStop { stopName :: String }
    | CmdInfo { infoName :: String }
    | CmdCdInsert { cdFile :: String, cdInsertName :: String}
    | CmdCdEject { cdEjectName :: String }
    | CmdAdd { addName :: String }
    | CmdSet { setName :: String, setField :: String }
    | CmdGet { getName :: String, getField :: String }
    | CmdConsole { consoleName :: String }
    | CmdCheck
    | CmdHelp
    deriving (Show,Eq)

getOpts :: IO Command
getOpts = execParser (info (parseCArgs <**> helper) idm)
  where parseCArgs = subparser $ mconcat $ map (\(name, v, desc) -> command name (info v (progDesc desc))) commands

commands =
    [ ("create", cmdCreate, "create a new VM")
    , ("list", cmdList, "list VMs")
    , ("start", cmdStart, "start a VM")
    , ("stop", cmdStop, "stop a VM")
    , ("info", cmdInfo, "info about a VM")
    , ("cd-insert", cmdCdInsert, "cd insert")
    , ("cd-eject", cmdCdEject, "cd eject")
    , ("add", cmdAdd, "add a property or value")
    , ("set", cmdSet, "set a value for a property")
    , ("get", cmdGet, "get a value for a property")
    , ("console", cmdConsole, "connect to the console")
    , ("check", pure CmdCheck, "perform check (for debug)")
    , ("help", pure CmdHelp, "get the help")
    ]
  where
        cmdCreate = CmdCreate <$> nameArg
        cmdList = pure CmdList
        cmdStart = CmdStart <$> flag False True (long "vnc" <> short 'v')
                            <*> flag False True (long "console" <> short 'c')
                            <*> nameArg
        cmdStop = CmdStop <$> nameArg
        cmdInfo = CmdInfo <$> nameArg
        cmdCdInsert = CmdCdInsert <$> strOption (long "file" <> short 'f' <> metavar "ISOFILE")
                                  <*> nameArg
        cmdCdEject = CmdCdEject <$> nameArg
        cmdAdd = CmdAdd <$> nameArg
        cmdSet = CmdSet <$> nameArg
                        <*> strOption (long "field" <> short 'f' <> metavar "FIELD")
        cmdGet = CmdGet <$> nameArg
                        <*> strOption (long "field" <> short 'f' <> metavar "FIELD")
        cmdConsole = CmdConsole <$> nameArg

        nameArg = argument Just (metavar "NAME")
