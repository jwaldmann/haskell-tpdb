module TPDB.DP.Graph where

import TPDB.DP.Unify
import TPDB.DP.Transform 

import TPDB.Data
import TPDB.Pretty

import TPDB.Plain.Read -- for testing
import TPDB.Plain.Write -- for testing

import qualified Data.Set as S
import Control.Monad ( guard, forM )
import Control.Applicative

import Control.Monad.State.Strict 

-- | replace each defined symbol, and each variable,
-- by a fresh distinct variable
rename_cap :: Ord c => S.Set c -> Term v c -> Term Int c
rename_cap defined t = evalState ( walk defined t ) 0

walk defined t = case t of
    Node f args | not $ S.member f defined 
        -> Node f <$> forM args ( walk defined )
    _ -> do i <- get ; put $ succ i ; return $ Var i

edges s = do
    let def = S.filter isOriginal $ defined s
    u <- filter strict $ rules s
    v <- filter strict $ rules s
    guard $ unifies ( vmap Left $ rename_cap def $ rhs u ) 
                    ( vmap Right $ rhs v )
    return (u,v)

check = edges $ dp trs0

Right trs0 = 
    TPDB.Plain.Read.trs "(VAR x y) (RULES f(S(x),y)->S(f(x,y)))" 
