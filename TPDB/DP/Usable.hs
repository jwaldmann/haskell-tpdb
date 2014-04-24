module TPDB.DP.Usable where

import TPDB.Data
import TPDB.Pretty

import TPDB.DP.Unify
import TPDB.DP.TCap

import qualified Data.Set as S

-- | DANGER: this ignores the CE condition
restrict :: (Ord c, Ord v) => RS c (Term v c) -> RS c (Term v c)
restrict dp = 
    dp { rules = filter strict (rules dp)
               ++ S.toList ( usable dp)
       }

-- | computes the least closed set of usable rules, cf. Def 4.5
-- http://cl-informatik.uibk.ac.at/users/griff/publications/Sternagel-Thiemann-RTA10.pdf

usable ::   (Ord v, Ord c)
       => TRS v c -> S.Set (Rule (Term v c))
usable dp = fixpoint ( \ s -> S.union s $ required dp s)
    (required dp $ S.fromList $ rules dp) 

fixpoint f x = 
    let y = f x in if x == y then x else fixpoint f y

required ::  (Ord v, Ord c)
       => TRS v c -> S.Set ( Rule (Term v c) ) ->  S.Set ( Rule (Term v c) ) 
required dp rs = 
    S.fromList $ do { r <- S.toList rs ;  needed dp $ rhs r }

needed :: (Ord v, Ord c)
       => TRS v c -> Term v c -> [ Rule (Term v c) ]
needed dp t = case t of
    Node f args -> 
          filter ( \ u -> unifies ( vmap Left $ lhs u ) ( vmap Right $ tcap dp t ) )
                ( filter (not . strict) $ rules dp )
        ++ ( args >>= needed dp )
    Var v -> []

