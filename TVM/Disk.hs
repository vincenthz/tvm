module TVM.Disk where

import Control.Monad (when)
import System.Process
import Data.Char (toLower)
import System.Exit

data DiskType = Qcow2 | Qcow | Raw
    deriving (Show,Eq)

createDisk :: FilePath -- ^ disk path
           -> Int      -- ^ size in megabytes
           -> DiskType -- ^ disk type
           -> IO ()
createDisk filepath sizeMb diskType = do
    ec <- rawSystem "qemu-img" cli
    when (ec /= ExitSuccess) $ do
        putStrLn (error "creating disk: " ++ show ec)
        exitFailure -- shouldn't exit, but just return error
  where cli = [ "create"
              , "-f", map toLower (show diskType)
              , filepath
              , show sizeMb ++ "M"
              ]
