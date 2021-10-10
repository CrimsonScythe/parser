{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_boa (
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
version = Version [0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\hasee\\Documents\\AP\\parser\\code\\part2\\.stack-work\\install\\ed09c254\\bin"
libdir     = "C:\\Users\\hasee\\Documents\\AP\\parser\\code\\part2\\.stack-work\\install\\ed09c254\\lib\\x86_64-windows-ghc-8.10.6\\boa-0.0.0-H5fTybuRNWn3vKvGUsORBa"
dynlibdir  = "C:\\Users\\hasee\\Documents\\AP\\parser\\code\\part2\\.stack-work\\install\\ed09c254\\lib\\x86_64-windows-ghc-8.10.6"
datadir    = "C:\\Users\\hasee\\Documents\\AP\\parser\\code\\part2\\.stack-work\\install\\ed09c254\\share\\x86_64-windows-ghc-8.10.6\\boa-0.0.0"
libexecdir = "C:\\Users\\hasee\\Documents\\AP\\parser\\code\\part2\\.stack-work\\install\\ed09c254\\libexec\\x86_64-windows-ghc-8.10.6\\boa-0.0.0"
sysconfdir = "C:\\Users\\hasee\\Documents\\AP\\parser\\code\\part2\\.stack-work\\install\\ed09c254\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "boa_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "boa_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "boa_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "boa_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "boa_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "boa_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
