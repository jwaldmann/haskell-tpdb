module TPDB.Input where

import TPDB.Data
import TPDB.Plain.Read
import TPDB.XTC.Read
import TPDB.Convert

import System.FilePath.Posix ( splitExtension )

-- | read input from file with given name.
-- can have extension .srs, .trs, .xml.

get :: FilePath 
         -> IO ( Either (TRS Identifier Identifier) 
                        ( SRS Identifier ) )
get f = do
    let ( base, ext ) = splitExtension f    
    case ext of                     
      ".srs" -> do
          s <- readFile f    
          case srs s of
              Left err -> error err
              Right t -> return $ Right t
      ".trs" -> do        
          s <- readFile f
          case TPDB.Plain.Read.trs s of
              Left err -> error err
              Right t -> return $ Left t            
      ".xml" -> do
          ps <- readProblems f
          case ps of 
              [ p ] -> return $ Left $ TPDB.Data.trs p

get_trs f = do
    x <- get f
    return $ case x of
        Right x -> srs2trs x
        Left  x -> x

get_srs f = do
    x <- get f
    return $ case x of
        Right x -> x
        Left  x -> case trs2srs x of
            Nothing -> error "not an SRS"
            Just x -> x
            

