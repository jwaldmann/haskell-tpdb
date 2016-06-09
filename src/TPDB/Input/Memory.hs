-- | read benchmark from in-memory data (e.g., from a ByteString)

module TPDB.Input.Memory

where

import TPDB.Data
import TPDB.Plain.Read
import TPDB.XTC.Read

import qualified Data.ByteString.Lazy as B
import System.FilePath.Posix ( takeExtension )

-- | first argument is file name, second argument is file contents.
-- first arg. is needed to pick the proper parser (SRS, TRS, XTC)
get :: String -> B.ByteString
    -> IO (Either String (Either (TRS Identifier Identifier) (SRS Identifier)))
get f s = case takeExtension f of
      ".srs" -> do
          case srs s of
              Left err -> return $ Left err
              Right t -> return $ Right $ Right t
      ".trs" -> do        
          case TPDB.Plain.Read.trs s of
              Left err -> return $ Left err
              Right t -> return $ Right $ Left t 
      _ -> do
          ps <- readProblemsBS s
          case ps of 
              [ p ] -> return $ Right $ Left $ TPDB.Data.trs p
              [] -> return $ Left "no TRS"
              _ -> return $ Left "more than one TRS"
