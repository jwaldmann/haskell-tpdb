{-# language DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module TPDB.Data.Term.Cached
  ( TermC, Term, pattern Var, pattern Node, tfold
  , size, depth, vars, syms
  )
where

import qualified Data.Set as S
import Data.Set (Set)
import Data.Typeable
import Data.Hashable
import GHC.Generics

data Term v s = Var_Imp
                { name  :: v
                -- , _hash :: !Int
                -- , size :: !Int
                -- , depth :: !Int
                -- , vars :: S.Set v
                -- , syms :: S.Set s
                }
              | Node_Imp
                { fun   :: s
                , args  :: [Term v s]
                -- , _hash :: !Int
                -- , size :: !Int
                -- , depth :: !Int
                -- , vars :: S.Set v
                -- , syms :: S.Set s
                }
    deriving ( Eq, Ord, Typeable, Generic )

vars :: TermC v c => Term v c -> S.Set v
vars = tfold S.singleton (\ _ -> S.unions)

syms :: TermC v c => Term v c -> S.Set c
syms = tfold (const S.empty) (\ f xs -> S.unions $ S.singleton f : xs)

size :: TermC v c => Term v c -> Int
size = tfold (const 0) (\ _ -> succ . sum )

depth :: TermC v c => Term v c -> Int
depth = tfold (const 0) (\ _ xs -> if null xs then 0 else succ $ maximum xs)

{-
instance TermC v s => Eq (Term v s) where
  s == t = hash s == hash t && case (s,t) of
    (Var x, Var y) -> x == y
    (Node f xs, Node g ys) -> (f,xs) == (g,ys)
    _ -> False
-}

-- | this will first compare hash values,
-- so the result is not predictable.
-- but it's fine for use with ordered sets etc.
{-
instance TermC v s => Ord (Term v s) where
  compare s t =
    case compare (hash s) (hash t) of
      EQ -> case (s,t) of
        (Var x, Var y) -> compare x y
        (Node f xs, Node g ys) -> compare (f,xs) (g,ys)
        (Var _, Node _ _) -> LT
        (Node _ _, Var _) -> GT
      c -> c  
-}

instance TermC v s => Hashable (Term v s)
  -- where hashWithSalt _ = _hash


pattern Var v = Var_Imp v
pattern Node f xs = Node_Imp f xs

{-               
pattern Var :: -- TermC v s => () =>
               v -> Term v s
pattern Var v <- Var_Imp { name = v } where
  Var v = Var_Imp { name = v
                  -- ,_hash = hash v
                  -- , size = 1, depth = 0
                  -- , vars = S.singleton v
                  -- , syms = mempty
                  }
-}

{-
pattern Node :: -- TermC v s => () =>
                s -> [Term v s] -> Term v s
pattern Node f xs <- Node_Imp { fun = f, args = xs } where
  Node f xs = Node_Imp { fun = f, args = xs
                       -- , _hash = hash (f, xs)
                       -- , size = 1 + sum (map size xs)
                       -- , depth = if null xs then 0 else succ $ maximum $ map depth xs
                       -- , vars = S.unions $ map vars xs
                       -- , syms = S.unions $ map syms xs
                       }
-}

type TermC v s = (Hashable v, Hashable s, Ord v, Ord s)

{-# INLINEABLE vmap #-}
vmap :: (TermC v s, TermC u s) => ( v -> u ) -> Term v s -> Term u s
vmap f = tfold (Var . f) Node

-- instance Functor ( Term v ) where
-- cannot instantiate Functor since we need TermC
{-# INLINEABLE tmap #-}
tmap f = tfold Var ( \ c xs -> Node (f c) xs)

{-# INLINE tfold #-}
tfold :: TermC v c => (v -> r) -> (c -> [r] -> r) -> Term v c -> r
tfold var node t =
  let go (Var v) = var v
      go (Node f xs) = node f (map go xs)
  in  go t

