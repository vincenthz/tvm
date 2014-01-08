{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Data.Data
import Data.Maybe
import Data.List
import Data.Aeson ()
import qualified Data.ByteString.Lazy as B
import System.FilePath
import System.Directory
import Control.Monad
import Control.Applicative

import System.Process

data BootOrder = BootDisk | BootCdrom
    deriving (Show,Read,Eq,Typeable,Data)
data DiskInterface = DiskSCSI | DiskIDE | DiskVirtIO
    deriving (Show,Read,Eq,Typeable,Data)
data DiskMedia = MediaDisk | MediaCDROM
    deriving (Show,Read,Eq,Typeable,Data)

data Disk = Disk
    { diskFile      :: String
    , diskInterface :: Maybe DiskInterface
    , diskMedia     :: DiskMedia
    } deriving (Show,Read,Eq,Typeable,Data)

data Nic = Nic
    { nicModel   :: Maybe String
    , nicMacAddr :: Maybe String -- FIXME refine type
    , nicVLAN    :: Maybe Int
    } deriving (Show,Read,Eq,Typeable,Data)

defaultNic :: Nic
defaultNic = Nic Nothing Nothing Nothing

data NetFamily = TCP | UDP
    deriving (Show,Read,Eq,Typeable,Data)

-- hostfwd=[tcp|udp]:[hostaddr]:hostport-[guestaddr]:guestport
data HostFWD = HostFWD
    { hostFWDFamily :: Maybe NetFamily
    , hostFWDHostAddr :: Maybe String
    , hostFWDHostPort :: Int
    , hostFWDGuestAddr :: Maybe String
    , hostFWDGuestPort :: Int
    } deriving (Show,Read,Eq,Typeable,Data)

data Net = NetUser
    { userNetHostFWD :: [HostFWD]
    } deriving (Show,Read,Eq,Typeable,Data)

data Serial = SerialPipe String
    deriving (Show,Read,Eq,Typeable,Data)

data VGA = VGA_Standard | VGA_Cirrus | VGA_QXL | VGA_VMWare | VGA_None
    deriving (Show,Read,Eq,Typeable,Data)

data QemuArch = ArchX86_64 | ArchI386
    deriving (Show,Read,Eq,Typeable,Data)

data Qemu = Qemu
    { qemuArch    :: QemuArch
    , qemuMemory  :: Int -- in megabytes
    , qemuCPUs    :: Int
    , qemuBoot    :: BootOrder
    , qemuDrives  :: [Disk]
    , qemuNics    :: [Nic]
    , qemuNets    :: [Net]
    , qemuSerials :: [Serial]
    , qemuVGA     :: VGA
    , qemuVNC     :: Maybe Int
    } deriving (Show,Read,Eq,Typeable,Data)

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
    }

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
    concatMap cliDisk (qemuDrives qemu) ++
    concatMap cliNic (qemuNics qemu) ++
    concatMap cliNet (qemuNets qemu) ++
    concatMap cliSerial (qemuSerials qemu)
  where cliDisk disk     = [ "-drive", kvs [ Just ("file", intercalate ",," $ wordsWhen (== ',') (resolveDir diskDir $ diskFile disk))
                                           , (\v -> ("if", toCLIArg v)) `fmap` diskInterface disk
                                           , Just ("media", toCLIArg $ diskMedia disk)
                                           ] ]
        cliNet net       = [ "-net", "user" `comma` (kvs $ map (\v -> Just ("hostfwd", toCLIArg v)) (userNetHostFWD net))]
        cliNic nic       = [ "-net", "nic" `comma` kvs [ (\v -> ("model", v)) `fmap` nicModel nic
                                                       , (\v -> ("vlan", show v)) `fmap` nicVLAN nic
                                                       , (\v -> ("macaddr", v)) `fmap` nicMacAddr nic
                                                       ] ]
        cliSerial (SerialPipe p) = [ "-serial", "pipe:" ++ (resolveDir serialDir p) ]
        kvs l = intercalate "," $ map (\(k,v) -> k ++ "=" ++ v) $ catMaybes l

        comma s v | null v    = s
                  | otherwise = s ++ "," ++ v

        wordsWhen     :: (Char -> Bool) -> String -> [String]
        wordsWhen p s =  case dropWhile p s of
                              "" -> []
                              s' -> w : wordsWhen p s''
                                    where (w, s'') = break p s'

resolveDir rDir path | isAbsolute path = path
                     | otherwise       = rDir </> path
