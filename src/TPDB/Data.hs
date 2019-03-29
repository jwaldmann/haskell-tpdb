-- | Data types for rewrite systems and termination problems.
-- A "bare" term rewrite system (list of rules and relative rules) is @TRS v s@.
-- A termination problem is @Problem v s@. This contains a rewrite system plus extra
-- information (strategy, theory, etc.)

{-# language DeriveDataTypeable,
    FlexibleInstances, FlexibleContexts,
    MultiParamTypeClasses, TypeFamilies
#-}

module TPDB.Data 

( module TPDB.Data
, module TPDB.Data.Term
, module TPDB.Data.Rule
)

where


import TPDB.Data.Term
import TPDB.Data.Rule
import TPDB.Data.Attributes

import Data.Typeable
import Control.Monad ( guard )

import Data.Hashable
import Data.Function (on)
import qualified Data.Text as T
import qualified Data.Set as S

data Identifier =
     Identifier { _identifier_hash :: ! Int
                , name :: ! T.Text
                , arity :: Int
                }
    deriving ( Eq, Ord, Typeable )

instance Hashable Identifier where
    hashWithSalt s i = hash (s, _identifier_hash i)

instance Show Identifier where show = T.unpack . name

mk :: Int -> T.Text -> Identifier
mk a n = Identifier { _identifier_hash = hash (a,n)
                    , arity = a, name = n }

class Ord (Var t) => Variables t where
  type Var t
  variables :: t -> S.Set (Var t)

instance Ord v => Variables (Term v c) where
  type Var (Term v c) = v
  variables = vars

instance Variables [c] where
  type Var [c] = ()
  variables _ = S.empty

---------------------------------------------------------------------

-- | according to XTC spec
data Funcsym = Funcsym
  { fs_name :: T.Text
  , fs_arity :: Int
  , fs_theory :: Maybe Theory
  , fs_replacementmap :: Maybe Replacementmap
  }
  deriving (Show, Typeable)

data Signature = Signature [ Funcsym ]
               | HigherOrderSignature
  deriving (Show, Typeable)

data Replacementmap = Replacementmap [Int]
  deriving (Show, Typeable)

---------------------------------------------------------------------


data RS s r =
     RS { signature :: [ s ] -- ^ better keep order in signature (?)
        , rules :: [ Rule r ]
        , separate :: Bool -- ^ if True, write comma between rules
        }
   deriving ( Typeable )

instance Eq r => Eq (RS s r) where
    (==) = (==) `on` rules

instance Functor (RS s) where
    fmap f rs = rs { rules = map (fmap f) $ rules rs }

strict_rules sys =
    do u <- rules sys ; guard $ strict u ; return ( lhs u, rhs u )
weak_rules sys =
    do u <- rules sys ; guard $ weak u ; return ( lhs u, rhs u )
equal_rules sys =
    do u <- rules sys ; guard $ equal u ; return ( lhs u, rhs u )

type TRS v s = RS s ( Term v s )

type SRS s = RS s [ s ]

instance Variables r => Variables (Rule r) where
  type Var (Rule r) = Var r
  variables u =
    S.unions [ variables (lhs u), variables (rhs u) ]

instance Ord v => Variables (TRS v s) where
  type Var (TRS v s) = v
  variables sys = S.unions $ map variables $ rules sys

data Problem v s =
     Problem { type_ :: Type
             , trs :: TRS v s
             , strategy :: Maybe Strategy
             , full_signature :: Signature
             -- , metainformation :: Metainformation
             , startterm :: Maybe Startterm
             , attributes :: Attributes
             }

data Type = Termination | Complexity
     deriving Show

data Strategy = Full | Innermost | Outermost
     deriving Show

-- | this is modelled after
-- https://www.lri.fr/~marche/tpdb/format.html
data Theorydecl v s
  = Property Theory [ s ] -- ^ example: "(AC plus)"
  | Equations [ Rule (Term v s) ]
    deriving Typeable

data Theory = A | C | AC
  deriving (Eq, Ord, Read, Show, Typeable)

data Startterm =
       Startterm_Constructor_based
       | Startterm_Full
     deriving Show

------------------------------------------------------

-- | legacy stuff (used in matchbox)

type TES = TRS Identifier Identifier
type SES = SRS Identifier

mknullary s = mk 0 s
mkunary   s = mk 1 s

from_strict_rules :: Bool -> [(t,t)] -> RS i t
from_strict_rules sep rs =
    RS { rules = map ( \ (l,r) ->
             Rule { relation = Strict, top = False, lhs = l, rhs = r } ) rs
       , separate = sep
       }

with_rules sys rs = sys { rules = rs }


