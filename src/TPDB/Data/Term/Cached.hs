{-# language DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module TPDB.Data.Term.Cached
( TermC, Term, pattern Var, pattern Node
  , tmap, vmap
  , size, depth
  , vars, lvars, voccs
  , syms, lsyms, symsl
  , subterms, strict_subterms
  , positions, varpos
  )
where

import qualified Data.Set as S
import Data.Set (Set)
import Data.Typeable
import Data.Hashable
import GHC.Generics

data Term v s = Var_Imp
                { name  :: !v
                -- , _hash :: !Int
                -- , size :: !Int
                -- , depth :: !Int
                -- , vars :: S.Set v
                -- , syms :: S.Set s
                }
              | Node_Imp
                { fun   :: !s
                , args  :: ![Term v s]
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


type Position = [ Int ]

positions :: TermC v c => Term v c 
          -> [ ( Position, Term v c ) ]
positions t = ( [], t ) : case t of
    Node c args -> do ( k, arg ) <- zip [ 0 .. ] args
                      ( p, s   ) <- positions arg
                      return ( k : p , s )
    _ -> []


-- | all positions
pos :: TermC v c => Term v c 
    -> [ Position ]
pos t = do
    ( p, s ) <- positions t
    return p

-- | non-variable positions
sympos :: TermC v c => Term v c 
    -> [ Position ]
sympos t = do
    ( p, Node {} ) <- positions t
    return p

-- | variable positions
varpos :: TermC v c => Term v c 
    -> [ Position ]
varpos t = if null (vars t) then [] else do
    ( p, Var {} ) <- positions t
    return p

-- | leaf positions (= nullary symbols)
leafpos :: TermC v c => Term v c 
    -> [ Position ]
leafpos t = do
    ( p, Node c [] ) <- positions t
    return p


{-# inline subterms #-}

-- | in preorder
subterms :: TermC v c => Term v c 
         -> [ Term v c ]
subterms t = t : case t of
    Node c args -> do arg <- args
                      subterms arg
    _ -> []

-- Note: following implementation relies on @subterms@
-- returning the preorder list (where the full term goes first)
strict_subterms t = tail $ subterms t

isSubtermOf :: (TermC v c, Eq c ) 
         => Term v c ->  Term v c  -> Bool
isSubtermOf s t =
  -- size s <= size t &&
  (elem s $ subterms t)

isStrictSubtermOf :: (TermC v c, Eq c ) 
         => Term v c ->  Term v c  -> Bool
isStrictSubtermOf s t =
  -- size s < size t &&
  (elem s $ strict_subterms t)

-- | compute new symbol at position, giving the position
pmap :: (TermC v c, TermC v d)
     =>( Position -> c -> d )
     -> Term v c
     -> Term v d
pmap f = rpmap ( \ p c -> f ( reverse p) c )

-- | compute new symbol from *reverse* position and previous symbol
-- this is more efficient (no reverse needed)
rpmap :: (TermC v c, TermC v d)
     => ( Position -> c -> d )
     -> Term v c
     -> Term v d
rpmap f t = helper [] t where
    helper p ( Node c args ) = Node ( f p c ) $ do
             ( k, arg ) <- zip [0..] args
             return $ helper ( k : p ) arg
    helper p ( Var v) = Var v



peek :: TermC v c
     => Term v c 
     -> Position 
     -> Term v c
peek t [] = t
peek ( Node c args ) ( k : ks ) = peek ( args !! k ) ks

peek_symbol :: TermC v c
     => Term v c 
     -> Position 
     -> c
peek_symbol t p = 
    case peek t p of
         Node c args -> c
         _ -> error "Autolib.TES.Position.peek_symbol called for non-symbol"

-- | warning: don't check arity
poke_symbol ::  TermC v c
     => Term v c 
     -> ( Position , c )
     -> Term v c
poke_symbol t ( p, c ) =  
    case peek t p of
         Node _ args -> poke t ( p, Node c args )
         _ -> error "Autolib.TES.Position.poke_symbol called for non-symbol"

poke :: TermC v c
     => Term v c 
     -> ( Position , Term v c )
     -> Term v c
poke t ( [], s ) = s
poke (Node c args) (k : ks, s ) = 
    let ( pre , this : post ) = splitAt k args
    in Node c ( pre ++ poke this ( ks, s ) : post )

pokes :: TermC v c
     => Term v c
      -> [ ( Position, Term v c ) ]
      -> Term v c
pokes = foldl poke


-- | list of function symbols (in pre-order, with duplicates)
symsl :: TermC v c
     => Term v c -> [ c ]
symsl t = do
    Node c _ <- subterms t
    return c

-- | unique
lsyms :: TermC v c => Term v c -> [ c ]
lsyms = S.toList . syms

isvar :: TermC v c => Term v c -> Bool
isvar ( Var _ ) = True ; isvar _ = False

-- | list of variables (each occurs once, unspecified ordering)
lvars :: TermC v c => Term v c -> [ v ]
lvars = S.toList . vars

-- | list of variables (in pre-order, with duplicates)
voccs :: TermC v c => Term v c -> [ v ]
voccs t = do ( p, Var v ) <- positions t ; return v

