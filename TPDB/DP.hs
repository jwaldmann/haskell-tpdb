module TPDB.DP where

import TPDB.Data
import TPDB.Pretty
import Text.PrettyPrint.HughesPJ

import qualified Data.Set as S
import Control.Monad ( guard )

data Marked a = Original a | Marked a | Auxiliary a
    deriving ( Eq, Ord )

instance Pretty a => Pretty ( Marked a) where
   pretty m = case m of
       Original a -> pretty a
       Marked a -> pretty a <> text "#"
       Auxiliary a -> pretty a

dp s = 
   let marked (Node f args) = 
          Node (Marked f) $ map (fmap Original) args
       os = map ( \ u -> Rule { relation = Weak
                               , lhs = fmap Original $ lhs u  
                               , rhs = fmap Original $ rhs u  
                               } )
           $ rules s
       defined = S.fromList $ do 
                u <- rules s
                let Node f args = lhs u 
                return f
       us = do 
            u <- rules s
            (_, r @ (Node f args)) <- positions $ rhs u
            guard $ S.member f defined
            return $ u { lhs = marked $ lhs u, rhs = marked r }
   in RS { rules = us ++ os, separate = separate s } 
