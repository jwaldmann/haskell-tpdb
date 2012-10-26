{-# language DeriveDataTypeable #-}

module TPDB.Data 

( module TPDB.Data
, module TPDB.Data.Term
)

where


import TPDB.Data.Term

import Data.Typeable
import Control.Monad ( guard )


data Identifier = Identifier { name :: String , arity :: Int }
    deriving ( Eq, Ord, Typeable )

instance Show Identifier where show = name

mk :: Int -> String -> Identifier
mk a n = Identifier { arity = a, name = n }

---------------------------------------------------------------------

data Relation = Strict |  Weak | Equal deriving ( Eq, Ord, Typeable, Show )

data Rule a = Rule { lhs :: a, rhs :: a 
                   , relation :: Relation
                   , top :: Bool
                   }
    deriving ( Eq, Ord, Typeable )

strict :: Rule a -> Bool
strict u = case relation u of Strict -> True ; _ -> False

weak :: Rule a -> Bool
weak u = case relation u of Weak -> True ; _ -> False

equal :: Rule a -> Bool
equal u = case relation u of Equal -> True ; _ -> False


data RS s r = 
     RS { signature :: [ s ] -- ^ better keep order in signature (?)
         , rules :: [ Rule r ]
        , separate :: Bool -- ^ if True, write comma between rules
         }
   deriving ( Typeable )

strict_rules sys = 
    do u <- rules sys ; guard $ strict u ; return ( lhs u, rhs u )
weak_rules sys = 
    do u <- rules sys ; guard $ weak u ; return ( lhs u, rhs u )
equal_rules sys = 
    do u <- rules sys ; guard $ equal u ; return ( lhs u, rhs u )

type TRS v s = RS s ( Term v s )

type SRS s = RS s [ s ]

data Problem v s = 
     Problem { type_ :: Type 
             , trs :: TRS v s
             , strategy :: Maybe Strategy
             -- , metainformation :: Metainformation
             , startterm :: Maybe Startterm  
             }

data Type = Termination | Complexity
     deriving Show 

data Strategy = Full | Innermost | Outermost
     deriving Show

data Startterm = 
       Startterm_Constructor_based
       | Startterm_Full
     deriving Show     

------------------------------------------------------

-- | legaca stuff (used in matchbox)

type TES = TRS Identifier Identifier
type SES = SRS Identifier

mknullary s = Identifier { arity = 0, name = s }
mkunary s = Identifier { arity = 1, name = s }

from_strict_rules :: Bool -> [(t,t)] -> RS i t
from_strict_rules sep rs = 
    RS { rules = map ( \ (l,r) ->
             Rule { relation = Strict, top = False, lhs = l, rhs = r } ) rs
       , separate = sep 
       }

with_rules sys rs = sys { rules = rs }


