{-# language DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module TPDB.Data.Term.Plain
( TermC, Term (..), tfold
  , size, depth, vars, syms
  )
where

import qualified Data.Set as S
import Data.Set (Set)
import Data.Typeable
import Data.Hashable
import GHC.Generics

data Term v s = Var v | Node s [Term v s]
    deriving ( Eq
             -- , Ord
             , Typeable, Generic )

{-# INLINE tfold #-}
tfold :: TermC v c => (v -> r) -> (c -> [r] -> r) -> Term v c -> r
tfold var node =
  let go (Var v) = var v
      go (Node f xs) = node f (map go xs)
  in  go

vars :: TermC v c => Term v c -> S.Set v
vars = tfold S.singleton (\ _ -> S.unions)

syms :: TermC v c => Term v c -> S.Set c
syms = tfold (const S.empty) (\ f xs -> S.unions $ S.singleton f : xs)

size :: TermC v c => Term v c -> Int
size = tfold (const 0) (\ _ -> succ . sum )

depth :: TermC v c => Term v c -> Int
depth = tfold (const 0) (\ _ xs -> if null xs then 0 else succ $ maximum xs)

instance TermC v s => Hashable (Term v s)

type TermC v s = (Hashable v, Hashable s, Ord v, Ord s)



