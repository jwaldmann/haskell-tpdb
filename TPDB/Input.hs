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
    m <- getE f
    case m of
        Right x -> return x 
        Left err -> error err

getE f = do
    let ( base, ext ) = splitExtension f    
    case ext of                     
      ".srs" -> do
          s <- readFile f    
          case srs s of
              Left err -> return $ Left err
              Right t -> return $ Right $ Right t
      ".trs" -> do        
          s <- readFile f
          case TPDB.Plain.Read.trs s of
              Left err -> return $ Left err
              Right t -> return $ Right $ Left t 
      ".xml" -> do
          ps <- readProblems f
          case ps of 
              [ p ] -> return $ Right $ Left $ TPDB.Data.trs p
              [] -> return $ Left "no TRS"
              _ -> return $ Left "more than one TRS"

get_trs f = do
    x <- get f
    return $ case x of
        Right x -> srs2trs x
        Left  x -> x

getE_trs f = do
    e <- getE f
    return $ case e of
        Right x -> Right $ case x of
            Right x -> srs2trs x
            Left  x -> x
        Left e -> Left e

get_srs f = do
    x <- get f
    return $ case x of
        Right x -> x
        Left  x -> case trs2srs x of
            Nothing -> error "not an SRS"
            Just x -> x
            

