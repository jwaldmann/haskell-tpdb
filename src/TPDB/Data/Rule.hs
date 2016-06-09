module TPDB.Data.Rule where

import Data.Typeable

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

instance Functor Rule where 
    fmap f u = u { lhs = f $ lhs u, rhs = f $ rhs u } 
