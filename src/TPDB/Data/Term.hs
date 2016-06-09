{-# language DeriveDataTypeable #-}

module TPDB.Data.Term where

import qualified Data.Set as S
import Data.Set (Set)
import Data.Typeable

data Term v s = Var v 
              | Node s [ Term v s ]
    deriving ( Eq, Ord, Show, Typeable )

vmap :: ( v -> u ) -> Term v s -> Term u s
vmap f ( Var v ) = Var ( f v )
vmap f ( Node c args ) = Node c $ map ( vmap f ) args

instance Functor ( Term v ) where
    fmap f ( Var v ) = Var v
    fmap f ( Node c args ) = Node (f c) ( map ( fmap f ) args )



type Position = [ Int ]

positions :: Term v c 
          -> [ ( Position, Term v c ) ]
positions t = ( [], t ) : case t of
    Node c args -> do ( k, arg ) <- zip [ 0 .. ] args
                      ( p, s   ) <- positions arg
                      return ( k : p , s )
    _ -> []

-- FIXME: inefficient implementation (walks the tree),
-- should store the result in each node instead,
-- but this would break pattern matching.
size :: Term v c -> Int
size t = length $ positions t

depth :: Term v c -> Int
depth t = case t of
  Var {} -> 0
  Node f args -> case args of
    [] -> 0
    _  -> 1 + maximum (map depth args)

-- | all positions
pos :: Term v c 
    -> [ Position ]
pos t = do
    ( p, s ) <- positions t
    return p

-- | non-variable positions
sympos :: Term v c 
    -> [ Position ]
sympos t = do
    ( p, Node {} ) <- positions t
    return p

-- | variable positions
varpos :: Term v c 
    -> [ Position ]
varpos t = do
    ( p, Var {} ) <- positions t
    return p

-- | leaf positions (= nullary symbols)
leafpos :: Term v c 
    -> [ Position ]
leafpos t = do
    ( p, Node c [] ) <- positions t
    return p


{-# inline subterms #-}

subterms :: Term v c 
         -> [ Term v c ]
subterms t = t : case t of
    Node c args -> do arg <- args
                      subterms arg
    _ -> []

-- Note: following implementation relies on @subterms@
-- returning the preorder list (where the full term goes first)
strict_subterms t = tail $ subterms t

isSubtermOf :: (Eq v, Eq c ) 
         => Term v c ->  Term v c  -> Bool
isSubtermOf s t = elem s $ subterms t

isStrictSubtermOf :: (Eq v, Eq c ) 
         => Term v c ->  Term v c  -> Bool
isStrictSubtermOf s t = elem s $ strict_subterms t

-- | compute new symbol at position, giving the position
pmap:: ( Position -> c -> d )
     -> Term v c
     -> Term v d
pmap f = rpmap ( \ p c -> f ( reverse p) c )

-- | compute new symbol from *reverse* position and previous symbol
-- this is more efficient (no reverse needed)
rpmap :: ( Position -> c -> d )
     -> Term v c
     -> Term v d
rpmap f t = helper [] t where
    helper p ( Node c args ) = Node ( f p c ) $ do
             ( k, arg ) <- zip [0..] args
             return $ helper ( k : p ) arg
    helper p ( Var v) = Var v



peek :: Term v c 
     -> Position 
     -> Term v c
peek t [] = t
peek ( Node c args ) ( k : ks ) = peek ( args !! k ) ks

peek_symbol :: Term v c 
     -> Position 
     -> c
peek_symbol t p = 
    case peek t p of
         Node c args -> c
         _ -> error "Autolib.TES.Position.peek_symbol called for non-symbol"

-- | warning: don't check arity
poke_symbol ::  Term v c 
     -> ( Position , c )
     -> Term v c
poke_symbol t ( p, c ) =  
    case peek t p of
         Node _ args -> poke t ( p, Node c args )
         _ -> error "Autolib.TES.Position.poke_symbol called for non-symbol"

poke :: Term v c 
     -> ( Position , Term v c )
     -> Term v c
poke t ( [], s ) = s
poke (Node c args) (k : ks, s ) = 
    let ( pre , this : post ) = splitAt k args
    in Node c ( pre ++ poke this ( ks, s ) : post )

pokes :: Term v c
      -> [ ( Position, Term v c ) ]
      -> Term v c
pokes = foldl poke


-- | in preorder 
symsl :: Term v c -> [ c ]
symsl t = do
    Node c _ <- subterms t
    return c

syms :: Ord c => Term v c -> Set c
syms = S.fromList . symsl


lsyms :: Ord c => Term v c -> [ c ]
lsyms = S.toList . syms

vars :: Ord v => Term v c -> Set v
vars t = S.fromList $ do
    Var v <- subterms t
    return v

isvar :: Term v c -> Bool
isvar ( Var _ ) = True ; isvar _ = False

-- | list of variables (each occurs once, unspecified ordering)
lvars :: Ord v => Term v c -> [ v ]
lvars = S.toList . vars

-- | list of variables (in pre-order, with duplicates)
voccs :: Term v c -> [ v ]
voccs t = do ( p, Var v ) <- positions t ; return v
