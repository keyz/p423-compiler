module Paths_p423_compiler (
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
version = Version [0,0,1] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/k/.cabal/bin"
libdir     = "/Users/k/.cabal/lib/x86_64-osx-ghc-7.8.4/p423-compiler-0.0.1"
datadir    = "/Users/k/.cabal/share/x86_64-osx-ghc-7.8.4/p423-compiler-0.0.1"
libexecdir = "/Users/k/.cabal/libexec"
sysconfdir = "/Users/k/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "p423_compiler_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "p423_compiler_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "p423_compiler_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "p423_compiler_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "p423_compiler_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
