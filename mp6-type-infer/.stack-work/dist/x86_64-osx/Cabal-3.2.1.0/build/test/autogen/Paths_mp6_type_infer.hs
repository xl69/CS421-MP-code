{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_mp6_type_infer (
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
version = Version [0,2,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/xl69/Desktop/CS421/mps/mp6-type-infer/.stack-work/install/x86_64-osx/17b0a9a637d547fa65684e36e4a18e24104b6f4d03ebf9d389867022bfe24524/8.10.3/bin"
libdir     = "/Users/xl69/Desktop/CS421/mps/mp6-type-infer/.stack-work/install/x86_64-osx/17b0a9a637d547fa65684e36e4a18e24104b6f4d03ebf9d389867022bfe24524/8.10.3/lib/x86_64-osx-ghc-8.10.3/mp6-type-infer-0.2.0.0-9uDJcFtH7cy6HdizrD3czK-test"
dynlibdir  = "/Users/xl69/Desktop/CS421/mps/mp6-type-infer/.stack-work/install/x86_64-osx/17b0a9a637d547fa65684e36e4a18e24104b6f4d03ebf9d389867022bfe24524/8.10.3/lib/x86_64-osx-ghc-8.10.3"
datadir    = "/Users/xl69/Desktop/CS421/mps/mp6-type-infer/.stack-work/install/x86_64-osx/17b0a9a637d547fa65684e36e4a18e24104b6f4d03ebf9d389867022bfe24524/8.10.3/share/x86_64-osx-ghc-8.10.3/mp6-type-infer-0.2.0.0"
libexecdir = "/Users/xl69/Desktop/CS421/mps/mp6-type-infer/.stack-work/install/x86_64-osx/17b0a9a637d547fa65684e36e4a18e24104b6f4d03ebf9d389867022bfe24524/8.10.3/libexec/x86_64-osx-ghc-8.10.3/mp6-type-infer-0.2.0.0"
sysconfdir = "/Users/xl69/Desktop/CS421/mps/mp6-type-infer/.stack-work/install/x86_64-osx/17b0a9a637d547fa65684e36e4a18e24104b6f4d03ebf9d389867022bfe24524/8.10.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "mp6_type_infer_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "mp6_type_infer_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "mp6_type_infer_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "mp6_type_infer_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "mp6_type_infer_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "mp6_type_infer_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
