module Paths_tpdb (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,2], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/tpdb-0.2/ghc-7.2.2"
datadir    = "/usr/local/share/tpdb-0.2"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "tpdb_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "tpdb_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "tpdb_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "tpdb_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
