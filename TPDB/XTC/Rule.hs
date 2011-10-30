module TPDB.XTC.Rule where

data Rule a = Rule { lhs :: a, rhs :: a 
                   , strict :: Bool
                   , top :: Bool
                   }
    deriving Show