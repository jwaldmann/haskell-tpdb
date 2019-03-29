{-# language LambdaCase #-}

module Main where

import TPDB.Input (get_srs)
import TPDB.Pretty
import TPDB.Plain.Write
import TPDB.Convert (srs2trs)
import System.IO (stdout)
import System.Environment

main = getArgs >>= \ case
  [ fp ] -> displayIO stdout . renderWide . pretty . srs2trs =<< get_srs fp
  
  