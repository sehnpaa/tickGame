{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_tickGame (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/peter/haskell/tickGame/.cabal-sandbox/bin"
libdir     = "/home/peter/haskell/tickGame/.cabal-sandbox/lib/x86_64-linux-ghc-8.4.4/tickGame-0.1.0.0-B1J9cUWYS9qHV6YBoP0cBg-tickGame"
dynlibdir  = "/home/peter/haskell/tickGame/.cabal-sandbox/lib/x86_64-linux-ghc-8.4.4"
datadir    = "/home/peter/haskell/tickGame/.cabal-sandbox/share/x86_64-linux-ghc-8.4.4/tickGame-0.1.0.0"
libexecdir = "/home/peter/haskell/tickGame/.cabal-sandbox/libexec/x86_64-linux-ghc-8.4.4/tickGame-0.1.0.0"
sysconfdir = "/home/peter/haskell/tickGame/.cabal-sandbox/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "tickGame_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "tickGame_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "tickGame_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "tickGame_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "tickGame_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "tickGame_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
