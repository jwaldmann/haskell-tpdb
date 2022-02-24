module TPDB.DP.Usable where

import TPDB.Data
import TPDB.Pretty

import TPDB.DP.Unify
import TPDB.DP.TCap

import qualified Data.Set as S
import qualified Data.Map.Strict as M

-- | DANGER: this ignores the CE condition
restrict :: TermC v c => RS c (Term v c) -> RS c (Term v c)
restrict dp = 
    dp { rules = filter strict (rules dp)
               ++ usable dp
       }

-- | computes the least closed set of usable rules, cf. Def 4.5
-- http://cl-informatik.uibk.ac.at/users/griff/publications/Sternagel-Thiemann-RTA10.pdf

usable :: TermC v c
       => TRS v c -> [Rule (Term v c)]
usable dp =
  let dpi = M.fromList $ zip [0..] $ rules dp
      fp = fixpoint
        ( \ s -> S.union s $ required dpi $ S.toList s)
        (required dpi $ map fst $ filter (strict . snd) $ M.toList dpi)
  in  map (dpi M.!) $ S.toList fp

fixpoint f x = 
    let y = f x in if x == y then x else fixpoint f y

-- | indices of rules that can be used
-- to rewrite rhs of rules with indices @is@
required :: TermC v c
       => M.Map Int ( Rule (Term v c) )
         -> [ Int ]
         -> S.Set Int
required dpi is =  S.fromList
  $ concatMap (needed dpi)
  $ map (rhs . (dpi M.!)) is

-- | indices of rules that can be used
-- to rewrite the given term @t@
needed :: TermC v c
       => M.Map Int (Rule (Term v c))
       -> Term v c
       -> [ Int ]
needed dpi t = case t of
    Node f args -> (map fst
         $ filter ( \ (j,u) -> unifies ( vmap Left $ lhs u ) ( vmap Right $ tcap (M.elems dpi) t ) )
         $ filter ( not . strict . snd)
         $ M.toList dpi)
        ++ ( args >>= needed dpi )
    Var v -> []

