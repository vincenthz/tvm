module TVM.Opts
    ( Command(..)
    , getOpts
    ) where

import Options.Applicative

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
    deriving (Show,Eq)

getOpts :: IO Command
getOpts = execParser (info (parseCArgs <**> helper) idm)
  where parseCArgs = subparser
            (  command "create" (info cmdCreate (progDesc "create a new VM"))
            <> command "list" (info cmdList (progDesc "list VMs"))
            <> command "start" (info cmdStart (progDesc "start a VM"))
            <> command "stop" (info cmdStop (progDesc "stop a VM"))
            <> command "info" (info cmdInfo (progDesc "info about a VM"))
            <> command "cd-insert" (info cmdCdInsert (progDesc "cd insert"))
            <> command "cd-eject" (info cmdCdEject (progDesc "cd eject"))
            <> command "add" (info cmdAdd (progDesc "add a property or value"))
            <> command "set" (info cmdSet (progDesc "set a value for a property"))
            <> command "get" (info cmdGet (progDesc "get a value for a property"))
            <> command "console" (info cmdConsole (progDesc "connect to the console"))
            )
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
