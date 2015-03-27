module TPDB.DP.Unify ( mgu, match, unifies, apply, times ) where

import TPDB.Data
import qualified Data.Map as M
import Control.Monad ( guard, foldM )
import Data.Maybe (isJust)

type Substitution v c = M.Map v (Term v c)

unifies t1 t2 = isJust $ mgu t1 t2

-- | view variables as symbols
pack :: Term v c -> Term any (Either v c)
pack ( Var v ) = Node ( Left v ) []
pack ( Node f args ) = Node ( Right f ) ( map pack args )

unpack :: Term any (Either v c) -> Term v c
unpack ( Node ( Left v ) [] ) = Var v
unpack ( Node ( Right f ) args ) = Node f ( map unpack args )

-- | will only bind variables in the left side
match :: ( Ord v, Ord w, Eq c )
      => Term v c
      -> Term w c
      -> Maybe ( M.Map v ( Term w c ) )
match l r = do
    u <- mgu ( fmap Right l ) ( pack r )
    return $ M.map unpack  u


-- | naive implementation (worst case exponential)
mgu
  :: (Ord v, Eq c) =>
     Term v c -> Term v c -> Maybe (M.Map v (Term v c))
mgu t1 t2 | t1 == t2 = return M.empty
mgu ( Var v ) t2 = do
    guard $ not $ elem (Var v) $ subterms t2
    return $ M.singleton v t2
mgu t1 ( Var v ) = mgu ( Var v ) t1  
mgu (Node f1 args1) (Node f2 args2) 
    | f1 == f2 && length args1 == length args2 = do
        guard $ f1 == f2
        foldM ( \ s (l,r) -> do
            t <- mgu (apply l s) (apply r s) 
            return $ times s t ) M.empty $ zip args1 args2 
mgu _ _ = Nothing
   
times :: Ord v 
      => Substitution v c -> Substitution v c -> Substitution v c
times s t = 
    M.union ( M.difference t s )
            ( M.map ( \ v -> apply v t ) s )

apply t s = case t of
    Var v -> case  M.lookup v s of Nothing -> t ; Just w -> w
    Node f args -> Node f $ map (\ a -> apply a s) args
    
