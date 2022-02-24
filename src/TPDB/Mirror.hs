module TPDB.Mirror where

import TPDB.Data
import TPDB.Convert

import Control.Monad ( forM, guard )

-- | if input is SRS, reverse lhs and rhs of each rule
mirror :: (Eq v, TermC v s)
  => TRS v  s 
       -> Maybe ( TRS v s )
mirror trs = do
    us <- forM (rules trs) $ \ u -> do
      ( left_spine, left_base ) <- spine $ lhs u
      ( right_spine, right_base ) <- spine $ rhs u
      guard $ left_base == right_base 
      return $ u { lhs = unspine left_base $ reverse left_spine
                 , rhs = unspine right_base $ reverse right_spine
                 }
    return $ trs { rules = us }
