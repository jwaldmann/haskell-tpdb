{-# language FlexibleContexts #-}

module TPDB.DP.TCap (tcap) where

import TPDB.Data
import TPDB.Pretty

import TPDB.DP.Unify

import Control.Monad (forM)
import Control.Monad.State.Strict 
import Control.Applicative


-- |  This function keeps only those parts of the input term which cannot be reduced,
-- even if the term is instantiated. All other parts are replaced by fresh variables.
-- Def 4.4 in http://cl-informatik.uibk.ac.at/users/griff/publications/Sternagel-Thiemann-RTA10.pdf

tcap :: (Ord v, Eq c, TermC v c) => [Rule (Term v c)] -> Term v c -> Term Int c
tcap dp t = evalState ( walk dp t ) 0

fresh_var :: TermC Int c => State Int ( Term Int c )
fresh_var = do i <- get ; put $! succ i ; return $ Var i

{-# INLINE walk #-}
walk dp =
  let go t = case t of
        Node f args -> do
          t' <- Node f <$> forM args go
          if all ( \ u -> not $ unifies ( vmap Left $ lhs u ) ( vmap Right t' ) )  $ filter (not . strict) dp 
            then return t' else fresh_var
        _ -> fresh_var 
  in  go
