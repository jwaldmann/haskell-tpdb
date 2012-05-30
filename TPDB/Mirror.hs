module TPDB.Mirror where

import TPDB.Data
import TPDB.Convert

-- | if input is SRS, reverse lhs and rhs of each rule
mirror :: TRS Identifier s 
       -> Maybe ( TRS Identifier s )
mirror trs = do
    srs <- trs2srs trs
    return $ srs2trs $ srs 
           { rules = for (rules srs ) $ \ u ->
                u { lhs = reverse $ lhs u
                  , rhs = reverse $ rhs u        
                  }        
           }     
  
for = flip map