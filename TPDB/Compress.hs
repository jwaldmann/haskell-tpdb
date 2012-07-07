{-# language TemplateHaskell #-}

module TPDB.Compress where

import TPDB.Data hiding ( trs, arity )
import TPDB.Plain.Write ()
import qualified TPDB.Plain.Read (trs) -- for testing
import TPDB.Pretty 

import Control.Monad ( guard ) 
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List ( sortBy )
import Data.Ord ( comparing )

-- | compute a compressed version of the TRS.
-- Warning: in the output, the arities of fresh identifiers will be nonsensical
compress :: ( Ord s )
         => [s] -- ^ supply of function symbols. This can be any infinite list,
                -- the implementation will filter out those elements that
                -- occur in the original TRS's signature.
         -> TRS v s 
         -> ( TRS v s 
            , [ ((s, Int), Maybe (s,Int,s,Int) ) ] 
            ) -- ^  ((f, a), Just (g,i,h,a)) semantics: new function symbols f
              -- by substituting h in the i-th position (start at 0) of g.
              -- arity of child is a
              -- ((f, a), Nothing ) semantics: f of arity a is an "old" function symbol.
              -- output is in dependency order (will only refer to previously defined symbols).
compress pool sys = 
    let osig = ohsig sys
        forbidden = M.keysSet osig
        fresh = filter ( `S.notMember` forbidden ) pool
        con = make fresh sys
    in  ( trs con 
        ,    map ( \ fa -> ( fa, Nothing ) ) ( M.toList osig )
          ++ map ( \ (f, p) -> ( (f, arity p), Just (parent p, branch p, child p, child_arity p) ) ) 
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
             , child_arity :: Int
             , has_grand_child :: Bool 
             }
    deriving ( Eq, Ord )

data Container v s = Container 
               { trs :: TRS v s
               , defs :: [( s, Pattern s)] 
               }

make fresh trs = handle fresh $ Container trs []


handle free con = 
    case -- take 1 $ 
         disjoint $ best_patterns $ trs con of
        [] -> con
        ps -> 
            let ( pre, post ) 
                    = splitAt ( length ps ) free
                here = zip pre ps      
            in handle post
                 $ con { trs = apply_system here $ trs con
                       , defs = here ++ defs con
                       }

patterns_in_term t = do
    Node f xs <- subterms t
    ( k , x @ ( Node g ys ) ) <- zip [ 0 .. ] xs
    let r = not $ null $ do Node {} <- ys ; return ()
    return $ Pattern { arity = length xs - 1 + length ys
                     , parent = f, branch = k
                     , child = g, child_arity = length ys
                     , has_grand_child = r 
                     }

patterns_in_rule u = do
    t <- [ lhs u, rhs u ]
    patterns_in_term t

patterns trs = do
    u <- rules trs
    patterns_in_rule u

disjoint ps =
   let h seen [] = []
       h seen (p:ps) = 
         if S.notMember (parent p) seen
            && S.notMember (child p) seen
         then p : h (S.insert (parent p)   
                     $ S.insert (child p) seen) ps
         else h seen ps     
   in  h S.empty ps 

best_patterns trs = do
    let pns = sortBy ( comparing ( negate . snd ))
            $  M.toList $ collect $ patterns trs
    let threshold = case pns of          
          [] -> 0
          (p,n) : _ -> div n 2
    (p,n) <- takeWhile ( \ (p,n) -> n >= threshold ) pns
    guard $ ( n > 1 ) || ( n == 1 && has_grand_child p )
    return p


apply_system fgps trs = do
    trs { rules = map ( apply_rule fgps ) $ rules trs }

apply_rule fgps u = 
    u { lhs = apply_term fgps $ lhs u
      , rhs = apply_term fgps $ rhs u
      }

apply_term _ ( Var v ) = Var v
apply_term fgps t @ (Node top args) = 
    let Node newtop newargs = multi_matches fgps t
    in  Node newtop $ map (apply_term fgps) newargs
        
multi_matches [] t = t
multi_matches ((fg, p@Pattern{parent=f,branch=i,child=g}) : fgps ) t@(Node top args) = 
    if matches p t
    then let ( pre, Node _ sub : post) = splitAt i args 
         in  Node fg ( pre ++ sub ++ post )    
    else multi_matches fgps t         

matches ( Pattern { parent = f, branch = i, child = g } ) 
        ( Node top args ) | top == f = 
    case args !! i of
         Node bot _ | bot == g -> True
         _ -> False
matches p _ = False



collect xs = M.fromListWith (+) $ do 
    x <- xs 
    return ( x, child_arity x )


invert :: ( Ord a, Ord b )
       => M.Map a b -> M.Map b [a]
invert fm = M.fromListWith (++) $ do
    ( k, v ) <- M.toList fm
    return ( v, [k] )
