module Paths_WordNumberTest (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/Scotty/Documents/Repositories/HaskellBookSolutions/chp14/WordNumberTest/.stack-work/install/x86_64-osx/lts-5.6/7.10.3/bin"
libdir     = "/Users/Scotty/Documents/Repositories/HaskellBookSolutions/chp14/WordNumberTest/.stack-work/install/x86_64-osx/lts-5.6/7.10.3/lib/x86_64-osx-ghc-7.10.3/WordNumberTest-0.1.0.0-6LEXdSF4Z8BJRKPrISJnv7"
datadir    = "/Users/Scotty/Documents/Repositories/HaskellBookSolutions/chp14/WordNumberTest/.stack-work/install/x86_64-osx/lts-5.6/7.10.3/share/x86_64-osx-ghc-7.10.3/WordNumberTest-0.1.0.0"
libexecdir = "/Users/Scotty/Documents/Repositories/HaskellBookSolutions/chp14/WordNumberTest/.stack-work/install/x86_64-osx/lts-5.6/7.10.3/libexec"
sysconfdir = "/Users/Scotty/Documents/Repositories/HaskellBookSolutions/chp14/WordNumberTest/.stack-work/install/x86_64-osx/lts-5.6/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "WordNumberTest_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "WordNumberTest_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "WordNumberTest_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "WordNumberTest_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "WordNumberTest_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
