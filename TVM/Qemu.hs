{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
-- |
-- Module      : TVM.Qemu
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : Linux
--
-- Qemu CLI wrapper for TVM
--
module TVM.Qemu where

import GHC.Generics

import Data.Data
import Data.Maybe
import Data.List
import Data.Char (toUpper)
import Data.Aeson (encode, eitherDecode)
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as B
import System.FilePath
import System.Directory
import Control.Monad
import Control.Applicative
import Control.DeepSeq

import Data.ByteString.Lazy.UTF8 (toString, fromString)

import System.Process

data BootOrder = BootDisk | BootCdrom
    deriving (Show,Read,Eq,Typeable,Data,Generic)
data DiskInterface = DiskSCSI | DiskIDE | DiskVirtIO
    deriving (Show,Read,Eq,Typeable,Data,Generic)
data DiskMedia = MediaDisk | MediaCDROM
    deriving (Show,Read,Eq,Typeable,Data,Generic)

data Disk = Disk
    { diskFile      :: !(Maybe String)
    , diskInterface :: !(Maybe DiskInterface)
    , diskMedia     :: !DiskMedia
    } deriving (Show,Read,Eq,Typeable,Data,Generic)

data Nic = Nic
    { nicModel   :: !(Maybe String)
    , nicMacAddr :: !(Maybe String) -- FIXME refine type
    , nicVLAN    :: !(Maybe Int)
    } deriving (Show,Read,Eq,Typeable,Data,Generic)

defaultNic :: Nic
defaultNic = Nic Nothing Nothing Nothing

data NetFamily = TCP | UDP
    deriving (Show,Read,Eq,Typeable,Data,Generic)

-- hostfwd=[tcp|udp]:[hostaddr]:hostport-[guestaddr]:guestport
data HostFWD = HostFWD
    { hostFWDFamily    :: !(Maybe NetFamily)
    , hostFWDHostAddr  :: !(Maybe String)
    , hostFWDHostPort  :: !Int
    , hostFWDGuestAddr :: !(Maybe String)
    , hostFWDGuestPort :: !Int
    } deriving (Show,Read,Eq,Typeable,Data,Generic)

data Net = NetUser
    { userNetHostFWD :: [HostFWD]
    } deriving (Show,Read,Eq,Typeable,Data,Generic)

data Serial = SerialPipe
    { serialName :: !String
    } deriving (Show,Read,Eq,Typeable,Data,Generic)

data VGA = VGA_Standard | VGA_Cirrus | VGA_QXL | VGA_VMWare | VGA_None
    deriving (Show,Read,Eq,Typeable,Data,Generic)

data QemuArch = ArchX86_64 | ArchI386
    deriving (Show,Read,Eq,Typeable,Data,Generic)

data MouseType = MouseNormal | MouseTablet
    deriving (Show,Read,Eq,Typeable,Data,Generic)

data Qemu = Qemu
    { qemuArch    :: !QemuArch
    , qemuMemory  :: !Int -- in megabytes
    , qemuCPUs    :: !Int
    , qemuBoot    :: !BootOrder
    , qemuDrives  :: [Disk]
    , qemuNics    :: [Nic]
    , qemuNets    :: [Net]
    , qemuSerials :: [Serial]
    , qemuVGA     :: !VGA
    , qemuVNC     :: !(Maybe Int)
    , qemuMouse   :: !MouseType
    } deriving (Show,Read,Eq,Typeable,Data,Generic)

defaultQemu = Qemu
    { qemuArch    = ArchX86_64
    , qemuMemory  = 1024
    , qemuCPUs    = 1
    , qemuBoot    = BootDisk
    , qemuDrives  = []
    , qemuNics    = []
    , qemuNets    = []
    , qemuSerials = []
    , qemuVGA     = VGA_Standard
    , qemuVNC     = Nothing
    , qemuMouse   = MouseNormal
    }

instance NFData Qemu where

instance ToJSON BootOrder
instance FromJSON BootOrder
instance ToJSON DiskInterface
instance FromJSON DiskInterface
instance ToJSON DiskMedia
instance FromJSON DiskMedia
instance ToJSON Disk
instance FromJSON Disk
instance ToJSON Nic
instance FromJSON Nic
instance ToJSON NetFamily
instance FromJSON NetFamily
instance ToJSON HostFWD
instance FromJSON HostFWD
instance ToJSON Net
instance FromJSON Net
instance ToJSON Serial
instance FromJSON Serial
instance ToJSON VGA
instance FromJSON VGA
instance ToJSON QemuArch
instance FromJSON QemuArch
instance ToJSON MouseType
instance FromJSON MouseType
instance ToJSON Qemu
instance FromJSON Qemu

parseCfg :: String -> Maybe Qemu
parseCfg = kvsToCfg . map kv . lines
  where kv s = case break (== '=') s of
                    (k,"")    -> error ("parsing cfg: wrong k=v format: " ++ s)
                    (k,'=':v) -> (strip k, strip v)
                    (k,_)     -> error ("internal error: break failed")

        strip s = dropSpaces $ reverse $ dropSpaces $ reverse s
          where dropSpaces = dropWhile (\c -> c == ' ' || c == '\t')

printCfg :: Qemu -> String
printCfg qemu = unlines $ map unKV $ cfgToKVS qemu
  where unKV (k,v) = k ++ " = " ++ v

cfgToKVS :: Qemu -> [(String, String)]
cfgToKVS (Qemu arch mem cpu boot drives nics nets serials vga vnc mouse) =
    [ ("arch", toCLIArg arch)
    , ("memory", show mem)
    , ("cpu", show cpu)
    , ("boot", drop 4 $ show boot)
    , ("vga", drop 4 $ show vga)
    , ("mouse", drop 5 $ show mouse)
    ]
    ++ mapi driveToKVs drives
    ++ mapi nicToKVs nics
    ++ mapi netToKVs nets
    ++ mapi serialToKVs serials
    ++ maybe [] (\port -> [ ("vnc", show port) ]) vnc
  where driveToKVs (i, drive)   = ("disk" ++ show i, toString $ encode drive)
        nicToKVs (i, nic)       = ("nic" ++ show i, toString $ encode nic)
        netToKVs (i, net)       = ("net" ++ show i, toString $ encode net)
        serialToKVs (i, serial) = ("serial" ++ show i, toString $ encode serial)
        mapi :: ((Int, a) -> (String, String)) -> [a] -> [(String, String)]
        mapi f l = map f $ zip [0..] l

kvsToCfg :: [(String, String)] -> Maybe Qemu
kvsToCfg kvs =
    case sequence $ map (flip lookup kvs) ["arch", "memory", "cpu", "boot", "vga"] of
        Nothing                             -> Nothing
        Just [arch, memory, cpu, boot, vga] ->
            Just $ Qemu { qemuArch   = read ("Arch" ++ map toUpper arch)
                        , qemuMemory = read memory
                        , qemuCPUs   = read cpu
                        , qemuBoot   = read ("Boot" ++ capitalize boot)
                        , qemuVGA    = read ("VGA_" ++ capitalize vga)
                        , qemuDrives = readPrefixI "disk"
                        , qemuNics   = readPrefixI "nic"
                        , qemuNets   = readPrefixI "net"
                        , qemuSerials= readPrefixI "serial"
                        , qemuVNC    = read <$> lookup "vnc" kvs
                        , qemuMouse  = maybe MouseNormal id $ fmap (\v -> read ("Mouse" ++ capitalize v)) $ lookup "mouse" kvs
                        }
        Just _ -> error "internal error in kvsToCfg"
  where capitalize []     = []
        capitalize (x:xs) = toUpper x : xs

        readPrefixI :: FromJSON a => String -> [a]
        readPrefixI pre = loop 0
          where loop :: FromJSON a => Int -> [a]
                loop i = case lookup (pre ++ show i) kvs of
                            Nothing -> []
                            Just s  -> case eitherDecode $ fromString s of
                                            Left err -> error ("decoding " ++ pre ++ show i ++ ":" ++ err)
                                            Right v  -> v : loop (i+1)

class ShowArg a where
    toCLIArg :: a -> String

instance ShowArg QemuArch where
    toCLIArg ArchX86_64 = "x86_64"
    toCLIArg ArchI386   = "i386"

instance ShowArg BootOrder where
    toCLIArg BootDisk = "c"
    toCLIArg BootCdrom = "d"

instance ShowArg DiskInterface where
    toCLIArg DiskSCSI   = "scsi"
    toCLIArg DiskIDE    = "ide"
    toCLIArg DiskVirtIO = "virtio"

instance ShowArg DiskMedia where
    toCLIArg MediaDisk  = "disk"
    toCLIArg MediaCDROM = "cdrom"

instance ShowArg VGA where
    toCLIArg VGA_Standard = "std"
    toCLIArg VGA_Cirrus   = "cirrus"
    toCLIArg VGA_QXL      = "qxl"
    toCLIArg VGA_VMWare   = "vmware"
    toCLIArg VGA_None     = "none"

instance ShowArg NetFamily where
    toCLIArg TCP = "tcp"
    toCLIArg UDP = "udp"

instance ShowArg HostFWD where
    -- hostfwd=[tcp|udp]:[hostaddr]:hostport-[guestaddr]:guestport
    toCLIArg hf = intercalate "-" $ map (intercalate ":")
                    [ [ maybe "" toCLIArg (hostFWDFamily hf)
                      , maybe "" id (hostFWDHostAddr hf)
                      , show (hostFWDHostPort hf)
                      ]
                    , [ maybe "" id (hostFWDGuestAddr hf)
                      , show (hostFWDGuestPort hf)
                      ]
                    ]

toCLI :: FilePath -> FilePath -> Qemu -> [String]
toCLI serialDir diskDir qemu =
    ["qemu-system-" ++ toCLIArg (qemuArch qemu)] ++
    ["-enable-kvm"] ++
    ["-m", show (qemuMemory qemu)] ++
    ["-smp", show (qemuCPUs qemu)] ++
    ["-boot", toCLIArg (qemuBoot qemu)] ++
    cliMouse (qemuMouse qemu) ++
    concatMap cliDisk (qemuDrives qemu) ++
    concatMap cliNic (qemuNics qemu) ++
    concatMap cliNet (qemuNets qemu) ++
    concatMap cliSerial (qemuSerials qemu)
  where cliDisk disk     = [ "-drive", kvs [ (\v -> ("file", intercalate ",," $ wordsWhen (== ',') $ resolveDir diskDir v)) `fmap` diskFile disk
                                           , (\v -> ("if", toCLIArg v)) `fmap` diskInterface disk
                                           , Just ("media", toCLIArg $ diskMedia disk)
                                           ] ]
        cliNet net       = [ "-net", "user" `comma` (kvs $ map (\v -> Just ("hostfwd", toCLIArg v)) (userNetHostFWD net))]
        cliNic nic       = [ "-net", "nic" `comma` kvs [ (\v -> ("model", v)) `fmap` nicModel nic
                                                       , (\v -> ("vlan", show v)) `fmap` nicVLAN nic
                                                       , (\v -> ("macaddr", v)) `fmap` nicMacAddr nic
                                                       ] ]
        cliMouse MouseNormal = []
        cliMouse MouseTablet = ["-usbdevice", "tablet"]

        cliSerial (SerialPipe p) = [ "-serial", "pipe:" ++ (resolveDir serialDir p) ]

        kvs l = intercalate "," $ map (\(k,v) -> k ++ "=" ++ v) $ catMaybes l

        comma s v | null v    = s
                  | otherwise = s ++ "," ++ v

        wordsWhen     :: (Char -> Bool) -> String -> [String]
        wordsWhen p s =  case dropWhile p s of
                              "" -> []
                              s' -> w : wordsWhen p s''
                                    where (w, s'') = break p s'

resolveDir :: FilePath -> FilePath -> FilePath
resolveDir rDir path | isAbsolute path = path
                     | otherwise       = rDir </> path
