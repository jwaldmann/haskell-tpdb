{-# language DeriveDataTypeable #-}

module TPDB.Data 

( module TPDB.Data
, module TPDB.Data.Term
)

where


import TPDB.Data.Term

import Data.Typeable



data Identifier = Identifier { name :: String  }
    deriving ( Eq, Ord, Show, Typeable )


---------------------------------------------------------------------

data Rule a = Rule { lhs :: a, rhs :: a 
                   , strict :: Bool
                   , top :: Bool
                   }
    deriving ( Eq, Show)

data RS s r = 
     RS { signature :: [ s ] -- ^ better keep order in signature (?)
         , rules :: [ Rule r ]
        , separate :: Bool -- ^ if True, write comma between rules
         }
    deriving Show

strict_rules = filter strict . rules
non_strict_rules = filter ( not . strict ) . rules

type TRS v s = RS s ( Term v s )

type SRS s = RS s [ s ]

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

---------------------------------------------------------------------

-- | legaca stuff (used in matchbox)

type TES = TRS Identifier Identifier
type SES = SRS Identifier

mknullary s = Identifier { name = s }
mkunary s = Identifier { name = s }

from_strict_rules :: Bool -> [(t,t)] -> RS i t
from_strict_rules sep rs = 
    RS { rules = map ( \ (l,r) -> Rule { strict = True, lhs = l, rhs = r } ) rs
       , separate = sep 
       }
with_rules sys rs = sys { rules = rs }


