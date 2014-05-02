module TVM.Disk where

import Control.Monad (when)
import System.Process
import Data.Char (toLower)
import System.Exit

data DiskType = Qcow2 | Qcow | Raw
    deriving (Show,Eq)

createDisk :: Maybe FilePath -- ^ optional backing file
           -> FilePath -- ^ disk path
           -> Int      -- ^ size in megabytes
           -> DiskType -- ^ disk type
           -> IO ()
createDisk backingFile filepath sizeMb diskType = do
    ec <- rawSystem "qemu-img" ("create" : cli)
    when (ec /= ExitSuccess) $ do
        putStrLn (error "creating disk: " ++ show ec)
        exitFailure -- shouldn't exit, but just return error
  where cli =
            maybe [] (\b -> ["-b", b]) backingFile ++
            [ "-f", map toLower (show diskType), filepath ] ++
            maybe [] (const [show sizeMb ++ "M"]) backingFile
