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
    let es = M.fromListWith (++) 
           $ do (p,q) <- edges s ; return (p, [q])
        key = M.fromList 
            $ zip (filter strict $ rules s) [0.. ]
    comp <- reverse $ stronglyConnComp $ do
        p <- M.keys key
        let qs = M.findWithDefault [] p es
        return (p, key M.! p, map (key M.!) qs )
    return $ case comp of
        CyclicSCC vs -> Right $ s { rules = vs 
                 ++ filter (not . strict) (rules s) } 
        AcyclicSCC v -> Left v

-- | edges of the estimated dependency graph
edges s = do
    let def = S.filter isOriginal $ defined s
    u <- filter strict $ rules s
    v <- filter strict $ rules s
    guard $ unifies ( vmap Left $ tcap s $ rhs u ) 
                    ( vmap Right $ lhs v )
    return (u,v)

check = edges $ dp sys

-- example from "DP Revisited" http://colo6-c703.uibk.ac.at/ttt/rta04.pdf
Right sys = 
    TPDB.Plain.Read.trs "(VAR x y) (RULES not(not(x)) -> x not(or(x,y)) -> and(not(x),not(y)) not(and(x,y)) -> or (not(x),not(y)) and(x,or(y,z)) -> or(and(x,z),and(y,z)) and(or(y,z),x) -> or(and(x,y),and(x,z)))"

