module TPDB.Input.File where

import TPDB.Data
import TPDB.Convert

import qualified TPDB.Input.Memory as TIM

import qualified Data.ByteString.Lazy as B
import System.FilePath.Posix ( takeExtension )

-- | read input from file with given name.
-- can have extension .srs, .trs, .xml.
-- unknown extension is considered as .xml, because of 
-- http://starexec.forumotion.com/t60-restore-file-extension-for-renamed-benchmarks

get :: FilePath 
         -> IO ( Either (TRS Identifier Identifier) 
                        ( SRS Identifier ) )
get f = do
    m <- getE f
    case m of
        Right x -> return x 
        Left err -> error err

getE f = do
  s <- B.readFile f
  TIM.get f s

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
            

