module TPDB.Data where

import Data.Set

data Identifier = Identifier { name :: String , arity :: Int }
    deriving Show

data Term v s = Var v 
              | Node s [ Term v s ]
    deriving Show

data Rule a = Rule { lhs :: a, rhs :: a 
                   , strict :: Bool
                   , top :: Bool
                   }
    deriving Show

data RS s r = 
     RS { signature :: [ s ] -- ^ better keep order in signature (?)
         , rules :: [ Rule r ]
        , separate :: Bool -- ^ if True, write comma between rules
         }
    deriving Show

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
