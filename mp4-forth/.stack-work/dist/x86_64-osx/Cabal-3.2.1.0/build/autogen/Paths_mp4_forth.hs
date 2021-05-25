{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_mp4_forth (
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

bindir     = "/Users/xl69/Desktop/CS421/mps/mp4-forth/.stack-work/install/x86_64-osx/29d86ed7d3ad881bb849de8cba475d0184d48985b4e6be1b120da3e2c17ca36c/8.10.3/bin"
libdir     = "/Users/xl69/Desktop/CS421/mps/mp4-forth/.stack-work/install/x86_64-osx/29d86ed7d3ad881bb849de8cba475d0184d48985b4e6be1b120da3e2c17ca36c/8.10.3/lib/x86_64-osx-ghc-8.10.3/mp4-forth-0.1.0.0-2PnBhnp6bh7FqvspKiYbDa"
dynlibdir  = "/Users/xl69/Desktop/CS421/mps/mp4-forth/.stack-work/install/x86_64-osx/29d86ed7d3ad881bb849de8cba475d0184d48985b4e6be1b120da3e2c17ca36c/8.10.3/lib/x86_64-osx-ghc-8.10.3"
datadir    = "/Users/xl69/Desktop/CS421/mps/mp4-forth/.stack-work/install/x86_64-osx/29d86ed7d3ad881bb849de8cba475d0184d48985b4e6be1b120da3e2c17ca36c/8.10.3/share/x86_64-osx-ghc-8.10.3/mp4-forth-0.1.0.0"
libexecdir = "/Users/xl69/Desktop/CS421/mps/mp4-forth/.stack-work/install/x86_64-osx/29d86ed7d3ad881bb849de8cba475d0184d48985b4e6be1b120da3e2c17ca36c/8.10.3/libexec/x86_64-osx-ghc-8.10.3/mp4-forth-0.1.0.0"
sysconfdir = "/Users/xl69/Desktop/CS421/mps/mp4-forth/.stack-work/install/x86_64-osx/29d86ed7d3ad881bb849de8cba475d0184d48985b4e6be1b120da3e2c17ca36c/8.10.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "mp4_forth_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "mp4_forth_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "mp4_forth_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "mp4_forth_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "mp4_forth_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "mp4_forth_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
