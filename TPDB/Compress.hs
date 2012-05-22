{-# language TemplateHaskell #-}

module TPDB.Compress where

import TPDB.Data hiding ( trs, arity )
import TPDB.Plain.Write ()
import qualified TPDB.Plain.Read (trs) -- for testing
import TPDB.Pretty 

import Control.Monad ( guard ) 
import qualified Data.Set as S
import qualified Data.Map as M

-- | compute a compresses version of the TRS.
-- Warning: in the output, the arities of fresh identifiers will we nonsensical
compress :: ( Ord s )
         => [s] -- ^ supply of function symbols. This can be any infinite list,
                -- the implementation will filter out those elements that
                -- occur in the original TRS's signature.
         -> TRS v s 
         -> ( TRS v s 
            , [ ((s, Int), Maybe (s,Int,s) ) ] 
            ) -- ^  ((f, a), Just (g,i,h)) semantics: new function symbols f
              -- by substituting h in the i-th position (start at 0) of g.
              -- ((f, a), Nothing ) semantics: f of arity a is an "old" function symbol.
              -- output is in dependency order (will only refer to previously defined symbols).
compress pool sys = 
    let osig = ohsig sys
        forbidden = M.keysSet osig
        fresh = filter ( `S.notMember` forbidden ) pool
        con = make fresh sys
    in  ( trs con 
        ,    map ( \ fa -> ( fa, Nothing ) ) ( M.toList osig )
          ++ map ( \ (f, p) -> ( (f, arity p), Just (parent p, branch p, child p) ) ) 
             (  reverse $ defs con )
        )

ohsig sys = M.fromListWith ( \ o n -> if o == n then o else error "different arities" ) 
             $ do u <- rules sys ; t <- [ lhs u , rhs u ]
                  ( _ , Node f args ) <- positions t
                  return ( f, length args )

dont_compress pool sys = 
    let osig = ohsig sys
    in ( sys , map ( \ fa -> ( fa, Nothing ) ) ( M.toList osig ) )
                  

data Pattern s = Pattern
             { arity :: Int
             , parent :: s
             , branch :: Int
             , child :: s
             , has_grand_child :: Bool 
             }
    deriving ( Eq, Ord )

data Container v s = Container 
               { trs :: TRS v s
               , defs :: [( s, Pattern s)] 
               }

make fresh trs = handle fresh $ Container trs []


handle ( fg : free ) con = 
    case best_patterns $ trs con of
        [] -> con
        p @ Pattern { parent = f, child = g } : _ -> 
            handle free  
                 $ con { trs = apply_system ( fg, p ) $ trs con
                       , defs = (fg, p) : defs con
                       }

patterns_in_term t = do
    Node f xs <- subterms t
    ( k , x @ ( Node g ys ) ) <- zip [ 0 .. ] xs
    let r = not $ null $ do Node {} <- ys ; return ()
    return $ Pattern { arity = length xs - 1 + length ys
                     , parent = f, branch = k, child = g, has_grand_child = r }

patterns_in_rule u = do
    t <- [ lhs u, rhs u ]
    patterns_in_term t

patterns trs = do
    u <- rules trs
    patterns_in_rule u

best_patterns trs = do
    let fm = invert $ collect $ patterns trs
    guard $ M.size fm > 0
    let ( n, ps ) = last $ M.toAscList fm
    p <- ps
    guard $ ( n > 1 ) || ( n == 1 && has_grand_child p )
    return p

-- apply_system :: ( Identifier, Pattern ) -> TES -> TES
apply_system fgp trs = do
    trs { rules = map ( apply_rule fgp ) $ rules trs }

apply_rule fgp u = 
    u { lhs = apply_term fgp $ lhs u
      , rhs = apply_term fgp $ rhs u
      }

{-
apply_term :: ( Identifier, Pattern ) 
           -> Term Identifier Identifier 
           -> Term Identifier Identifier 
-}
apply_term ( fg, p ) ( Var v ) = Var v
apply_term ( fg, p @ Pattern { parent = f, branch = i, child = g } ) 
           t @ ( Node top args ) =
    let ( newtop, newargs ) =
            if matches p t 
            then let ( pre, Node _ sub : post ) = splitAt i args
                 in  ( fg, pre ++ sub ++ post )
            else ( top, args )
    in  Node newtop $ map ( apply_term ( fg, p ) ) newargs

matches ( Pattern { parent = f, branch = i, child = g } ) 
        ( Node top args ) | top == f = 
    case args !! i of
         Node bot _ | bot == g -> True
         _ -> False
matches p _ = False



collect xs = M.fromListWith (+)  $ zip xs $ repeat 1

invert :: ( Ord a, Ord b )
       => M.Map a b -> M.Map b [a]
invert fm = M.fromListWith (++) $ do
    ( k, v ) <- M.toList fm
    return ( v, [k] )
