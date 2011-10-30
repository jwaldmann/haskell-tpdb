module TPDB.XTC.RS where

import TPDB.XTC.Identifier
import TPDB.XTC.Term
import TPDB.XTC.Rule

import Data.Set

data RS s r = 
     RS { signature :: [ s ] -- ^ better keep order in signature (?)
         , rules :: [ Rule r ]
        , separate :: Bool -- ^ if True, write comma between rules
         }
    deriving Show

type TRS v s = RS s ( Term v s )

type SRS s = RS s [ s ]
