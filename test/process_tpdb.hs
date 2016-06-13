{-# language OverloadedStrings #-}

import TPDB.Data
import TPDB.XTC.Read

import qualified Data.ByteString.Lazy as BS
import qualified Codec.Archive.Tar as T
import qualified Codec.Compression.GZip as GZip
import System.FilePath
import System.Environment

main = do
  [f] <- getArgs
  s <- BS.readFile f
  handle $ T.read $ GZip.decompress s

handle ar = case ar of
  T.Done -> putStrLn "done"
  T.Next e ar' -> do
    print $ T.entryPath e
    case takeExtension $ T.entryPath e of
      ".xml" -> case T.entryContent e of
        T.NormalFile s n -> do
          [p] <- readProblemsBS s
          print $ attributes p
      _ -> return ()
    handle ar'
