--
-- Copyright (C) 2013 Vincent Hanquez <vincent@snarc.org>
-- License: BSD3
--
-- Tiny VM Manager
--
module Main where

import TVM.Qemu
import Data.Data
import Data.Maybe
import Data.List
import Data.List.Split
import Data.Aeson as DA (encode, eitherDecode)
import qualified Data.ByteString.Lazy as B
import System.Environment
import System.Exit
import System.Console.GetOpt
import System.FilePath
import System.Directory
import Control.Monad
import Control.Applicative
import Text.Groom

import System.IO
import System.Process
import System.Posix.Process (forkProcess, executeFile, getProcessStatus, getProcessID)
import System.Posix.Files (createNamedPipe, getFileStatus, isNamedPipe)

type Name = String

pidPath tvm name = runningDir tvm </> (name ++ ".pid")
cfgPath tvm name = configDir tvm </> (name ++ ".cfg")

readCfg :: TVMConfig -> Name -> IO (Maybe Qemu)
readCfg tvm name = do
    d <- (DA.eitherDecode <$> B.readFile (cfgPath tvm name)) :: IO (Either String Qemu)
    case d of
      Left err -> error $ "JSON error (" ++ (cfgPath tvm name) ++ "): " ++ err
      Right ps -> return $ Just ps

withCfg tvm name f = readCfg tvm name >>= maybe (return $ error ("cannot open config : " ++ name)) f

writeCfg tvm name config = B.writeFile (cfgPath tvm name) (DA.encode config)

listCfg tvm = do
    files <- filter (isSuffixOf ".cfg") <$> getDirectoryContents (configDir tvm)
    return $ sort $ map (reverse . drop 4 . reverse) files

readPid :: TVMConfig -> String -> IO Int
readPid tvm name =
   read <$> readFile (pidPath tvm name)

deletePid tvm name = removeFile (pidPath tvm name)

getVMPid :: TVMConfig -> String -> IO (Maybe Int)
getVMPid tvm name = do
    pidFileExist <- doesFileExist (pidPath tvm name)
    if pidFileExist
        then do pid <- read <$> readFile (pidPath tvm name)
                dirExist <- doesDirectoryExist ("/proc/" ++ show pid)
                if dirExist
                    then do
                        cmdline <- readFile ("/proc/" ++ show pid ++ "/cmdline")
                        if "qemu-" `isPrefixOf` cmdline
                            then return $ Just pid 
                            else return Nothing
                    else return Nothing
        else return Nothing

withRunningVM tvm name f = do
    mpid <- getVMPid tvm name
    case mpid of
        Nothing  -> putStrLn (name ++ " is not running")
        Just pid -> f pid

isRunning tvm name = maybe False (const True) <$> getVMPid tvm name
 
data TVMConfig = TVMConfig
    { rootDir    :: FilePath
    , diskDir    :: FilePath
    , configDir  :: FilePath
    , consoleDir :: FilePath
    , runningDir :: FilePath
    }

doCreate tvm args =
    case args of
        name:[] -> do
            names <- listCfg tvm
            when (name `elem` names) $ error ("config " ++ name ++ " already exist")
            let cfg = defaultQemu
                    { qemuDrives = [ Disk { diskFile = diskDir tvm </> (name ++ ".dsk"), diskInterface = Just DiskVirtIO, diskMedia = MediaDisk }
                                   , Disk { diskFile = "isoFile.iso", diskInterface = Nothing, diskMedia = MediaCDROM }
                                   ]
                    , qemuNics = [ defaultNic { nicModel = Just "e1000" } ]
                    , qemuNets = [ NetUser { userNetHostFWD = [ HostFWD Nothing Nothing 4022 Nothing 22 ] } ]
                    , qemuSerials = [ SerialPipe name ]
                    }
            writeCfg tvm name cfg
        _       -> error "usage: tvm create <name>"


doList tvm args = do
    names <- listCfg tvm
    forM_ names $ \name -> do
        pid <- getVMPid tvm name
        putStrLn (name ++ (maybe " [stopped]" (\p -> "[running pid=" ++ show p ++ "]") pid))

data StartOpts = StartVNC | StartConsole
    deriving (Show,Eq)

doStart tvm args =
    case getOpt Permute options args of
        (o,n,[]  ) -> start o n
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo "usage: tvm start [--vnc] <name>" options))
  where options =
            [ Option ['v'] ["vnc"] (NoArg StartVNC) "start a vnc viewer"
            , Option ['c'] ["console"] (NoArg StartConsole) "start the console"
            ]
        -- actual start
        start opts [name] = withCfg tvm name $ \cfg -> do
            let cmds = toCLI (consoleDir tvm) (diskDir tvm) cfg
            let pidFile = pidPath tvm name
            -- it's of course racy..
            mpid <- getVMPid tvm name
            when (isJust mpid) $ error (name ++ " is already running")

            prepareSerials cfg

            pid <- forkProcess $ startProcess pidFile cmds
            _   <- getProcessStatus False False pid

            -- FIXME start a vnc viewer
            when (StartVNC `elem` opts) $ return ()
            when (StartConsole `elem` opts) $ doConsole tvm [name]
            exitSuccess
        start _ _ = error "usage: tvm start <name>"

        startProcess pidFile (cmd:args) = do
            pid <- forkProcess $ exec pidFile cmd args
            _   <- getProcessStatus False False pid
            exitSuccess
        exec pidFile cmd args = do
            myPid <- getProcessID
            writeFile pidFile (show myPid)
            putStrLn ("starting qemu as pid: " ++ show myPid)
            executeFile cmd True args Nothing

        -- check if all serials file are available, and if not create them
        prepareSerials cfg = mapM_ checkFifos $ concatMap prep $ qemuSerials cfg
          where prep (SerialPipe path) =
                    let p = resolveDir (consoleDir tvm) path
                     in [p ++ ".in", p ++ ".out"]
                checkFifos path = do
                    exists <- doesFileExist path
                    if exists
                        then do
                            fstat <- getFileStatus path
                            if isNamedPipe fstat
                                then return ()
                                else error ("serial " ++ path ++ " already exist but is not a fifo")
                        else createNamedPipe path 0o0700

doStop tvm args = do
    case args of
        [name] -> withRunningVM tvm name $ \pid -> do
                     deletePid tvm name
                     ec <- rawSystem "kill" [ show pid ]
                     exitWith ec
        _ -> error "usage: tvm stop <name>"

doAdd tvm name args = undefined
doSet tvm name args = undefined

doGetQemu cfg ["qemuArch"]            = show $ qemuArch cfg
doGetQemu cfg ["qemuMemory"]          = show $ qemuMemory cfg
doGetQemu cfg ["qemuCPUs"]            = show $ qemuCPUs cfg
doGetQemu cfg ["qemuBoot"]            = show $ qemuBoot cfg
doGetQemu cfg ["qemuDrives"]          = show $ length $ qemuDrives cfg
doGetQemu cfg ["qemuDrives",index]    = show $ (qemuDrives cfg)!!(read index)
doGetQemu cfg ("qemuDrives":index:xs) =
    doGetQemuDrive ((qemuDrives cfg)!!(read index)) xs
    where doGetQemuDrive driveCfg ["diskFile"]      = show $ diskFile driveCfg
          doGetQemuDrive driveCfg ["diskInterface"] = show $ diskInterface driveCfg
          doGetQemuDrive driveCfg ["diskMedia"]     = show $ diskMedia driveCfg
          doGetQemuDrive _ list                     = "not supported"
doGetQemu cfg ["qemuNics"]            = show $ length $ qemuNics cfg
doGetQemu cfg ["qemuNics",index]      = show $ (qemuNics cfg)!!(read index)
doGetQemu cfg ("qemuNics":index:xs)   =
    doGetQemuNics ((qemuNics cfg)!!(read index)) xs
    where doGetQemuNics nicCfg ["nicModel"]   = show $ nicModel nicCfg
          doGetQemuNics nicCfg ["nicMacAddr"] = show $ nicMacAddr nicCfg
          doGetQemuNics nicCfg ["nicVLAN"]    = show $ nicVLAN nicCfg
          doGetQemuNics _      list           = "not supported"
doGetQemu cfg ["qemuNets"]            = show $ length $ qemuNets cfg
doGetQemu cfg ["qemuNets",index]      = show $ (qemuNets cfg)!!(read index)
doGetQemu cfg ("qemuNets":index:xs)   =
    doGetQemuNet ((qemuNets cfg)!!(read index)) xs
    where doGetQemuNet netCfg ["userNetHostFWD"]      = show $ length $ userNetHostFWD netCfg
          doGetQemuNet netCfg ["userNetHostFWD",i]    = show $ (userNetHostFWD netCfg)!!(read i)
          doGetQemuNet netCfg ("userNetHostFWD":i:xs) = doGetQemuNetFWD ((userNetHostFWD netCfg)!!(read i)) xs
          doGetQemuNet _      list                    = "not supported"

          doGetQemuNetFWD hostFwdCfg ["hostFWDFamily"]    = show $ hostFWDFamily hostFwdCfg
          doGetQemuNetFWD hostFwdCfg ["hostFWDHostAddr"]  = show $ hostFWDHostAddr hostFwdCfg
          doGetQemuNetFWD hostFwdCfg ["hostFWDHostPort"]  = show $ hostFWDHostPort hostFwdCfg
          doGetQemuNetFWD hostFwdCfg ["hostFWDGuestAddr"] = show $ hostFWDGuestAddr hostFwdCfg
          doGetQemuNetFWD hostFwdCfg ["hostFWDGuestPort"] = show $ hostFWDGuestPort hostFwdCfg
          doGetQemuNetFWD _          list                 = "not supported"
doGetQemu cfg ["qemuSerials"]         = show $ length $ qemuSerials cfg
doGetQemu cfg ["qemuSerials",index]   = show $ (qemuSerials cfg)!!(read index)
doGetQemu cfg ["qemuVGA"]             = show $ qemuVGA cfg
doGetQemu cfg ["qemuVNC"]             = show $ qemuVNC cfg
doGetQemu _   list                    = "not supported"

doGet tvm name args = do
    case args of
        [field] -> withCfg tvm name $ \cfg -> do
                          let splittedList = splitOn "." field
                          putStrLn $ doGetQemu cfg splittedList
        _ -> error $ "usage: tvm get <name> <field>"

doInfo tvm name args = withCfg tvm name $ \cfg -> do
    field "arch  " (qemuArch cfg)
    field "memory" (qemuMemory cfg)
    field "cpus  " (qemuCPUs cfg)
    field "boot  " (qemuBoot cfg)
    field "vga   " (qemuVGA cfg)
    list "drives" (qemuDrives cfg) $ \disk ->
        putStrLn ("file=" ++ diskFile disk ++ " if=" ++ show (diskInterface disk) ++ " media=" ++ show (diskMedia disk))
    list "nics" (qemuNics cfg) (\nic -> putStrLn $ groom nic)
    list "nets" (qemuNets cfg) (\net -> putStrLn $ groom net)
    list "serials" (qemuSerials cfg) (\net -> putStrLn $ groom net)
  where field k v = putStrLn (k ++ ": " ++ show v)
        list name l printer = putStrLn name >> mapM_ (\(i,f) -> putStr ("  [" ++ show i ++ "] ") >> printer f) (zip [0..] l)

doConsole tvm args =
    case args of
        [name] -> withRunningVM tvm name $ \_ ->
                  withCfg tvm name $ \cfg -> do
                      let path = case qemuSerials cfg of
                                    SerialPipe p:_ -> resolveDir (consoleDir tvm) p ++ ".out"
                                    []             -> error "no serial defined"
                      withFile path ReadMode $ \h ->
                          B.hGetContents h >>= B.putStr 
                      return ()
        _ -> error "usage: tvm stop <name>"

doCdInsert tvm file args =
    undefined

doCdEject tvm args =
    undefined

main = do
    home <- getHomeDirectory
    let tvmDir = home </> ".tvm"
        tvm = TVMConfig { rootDir    = tvmDir
                        , configDir  = tvmDir </> "config"
                        , diskDir    = tvmDir </> "disk"
                        , consoleDir = tvmDir </> "console"
                        , runningDir = tvmDir </> "running"
                        }
    dirExist <- doesDirectoryExist (rootDir tvm)
    unless dirExist $ do
        mapM_ createDirectory $ map (flip ($) tvm) [rootDir, configDir, diskDir, consoleDir, runningDir]
    args <- getArgs
    case args of
        "create":xs -> doCreate tvm xs
        "list":xs   -> doList tvm xs
        "start":xs  -> doStart tvm xs
        "stop":xs   -> doStop tvm xs
        "info":name:xs -> doInfo tvm name xs
        "cd-insert":file:xs -> doCdInsert tvm file xs
        "cd-eject":xs -> doCdEject tvm xs
        "add":name:xs -> doSet tvm name xs
        "set":name:xs -> doSet tvm name xs
        "get":name:xs -> doGet tvm name xs
        "console":xs -> doConsole tvm xs
        "info":[]  -> error "need a name"
        "add":[]   -> error "need a name"
        "set":[]   -> error "need a name"
        "get":[]   -> error "need a name"
        _:_        -> error "unknown command"
        []         -> error "no argument"
