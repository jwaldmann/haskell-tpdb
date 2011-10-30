-- | algebraic data type representing termination problems,
-- see http://www.termination-portal.org/wiki/XTC_Format_Specification

module TPDB.XTC.Problem where


import TPDB.XTC.Term
import TPDB.XTC.RS

data Problem v s = 
     Problem { trs :: TRS v s
             , strategy :: Strategy
             -- , metainformation :: Metainformation
             , type_ :: Type 
             }
     deriving Show

data Type = Termination | Complexity
     deriving Show 

data Strategy = Full | Innermost | Outermost
     deriving Show


