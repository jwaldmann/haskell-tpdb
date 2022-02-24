{-# language OverloadedStrings #-}

module TPDB.DP.Graph where

import TPDB.DP.TCap
import TPDB.DP.Unify
import TPDB.DP.Transform 

import TPDB.Data
import TPDB.Pretty

import TPDB.Plain.Read -- for testing
import TPDB.Plain.Write -- for testing

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Graph ( stronglyConnComp, SCC(..) )
import Control.Monad ( guard, forM )
import Control.Applicative

import Control.Monad.State.Strict 


-- | DP problems for strongly connected components, 
-- topologically sorted, with CyclicComponents in Right,
-- others in Left.
components s = do 
    let su = indexed s
        es = M.fromListWith (<>) 
           $ do (i,j) <- edges su ; return (i, S.singleton j)
    comp <- reverse $ stronglyConnComp $ do
        (i,u) <- M.toList su
        let js = M.findWithDefault mempty i es
        return (u, i, S.toList js)
    return $ case comp of
        CyclicSCC vs -> Right $ s { rules = vs 
                 ++ filter (not . strict) (rules s) } 
        AcyclicSCC v -> Left v

-- | edges of the estimated dependency graph
edges su = do
    (i,u) <- M.toList su
    (j,v) <- M.toList su
    guard $ unifies ( vmap Left $ tcap (M.elems su) $ rhs u ) 
                    ( vmap Right $ lhs v )
    return (i,j)

check = edges $ indexed $ dp sys

-- | numbering for non-strict rules
indexed :: TRS v c -> M.Map Int (Rule (Term v c))
indexed s = M.fromList $ zip [0::Int ..] $ filter strict $ rules s

-- example from "DP Revisited" http://colo6-c703.uibk.ac.at/ttt/rta04.pdf
Right sys = 
    TPDB.Plain.Read.trs "(VAR x y) (RULES not(not(x)) -> x not(or(x,y)) -> and(not(x),not(y)) not(and(x,y)) -> or (not(x),not(y)) and(x,or(y,z)) -> or(and(x,z),and(y,z)) and(or(y,z),x) -> or(and(x,y),and(x,z)))"

