module TPDB.Data.Term (module T, module TPDB.Data.Term) where

import TPDB.Data.Term.Plain as T
-- import TPDB.Data.Term.Cached as T

import qualified Data.Set as S


{-# INLINEABLE vmap #-}
vmap :: (TermC v s, TermC u s) => ( v -> u ) -> Term v s -> Term u s
vmap f = tfold (Var . f) Node

-- instance Functor ( Term v ) where
-- cannot instantiate Functor since we need TermC
{-# INLINEABLE tmap #-}
tmap f = tfold Var ( \ c xs -> Node (f c) xs)


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


-- | in preorder
{-# INLINE subterms #-}
subterms :: TermC v c => Term v c 
         -> [ Term v c ]
subterms t = t : case t of
    Node c args -> args >>= subterms
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
symsl :: TermC v c  => Term v c -> [ c ]
symsl t = do Node c _ <- subterms t; return c

-- | unique
lsyms :: TermC v c => Term v c -> [ c ]
lsyms = S.toList . syms

isvar :: TermC v c => Term v c -> Bool
isvar ( Var _ ) = True ; isvar _ = False

-- | list of variables (each occurs once, unspecified ordering)
lvars :: TermC v c => Term v c -> [ v ]
lvars = S.toList . vars

-- | list of variables (in pre-order, with duplicates)
{-# INLINE voccs #-}
voccs :: TermC v c => Term v c -> [ v ]
voccs = tfold (\ v -> [v]) (\ _ -> concat)

