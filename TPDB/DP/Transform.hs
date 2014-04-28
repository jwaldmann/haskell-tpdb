{-# language OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module TPDB.DP.Transform  where

import TPDB.Data
import TPDB.Pretty

import qualified Data.Set as S
import Control.Monad ( guard, forM )

import Data.Hashable
import GHC.Generics

data Marked a = Original a | Marked a | Auxiliary a
    deriving ( Eq, Ord, Generic )

isOriginal m = case m of Original {} -> True ; _ -> False
isMarked   m = case m of Marked   {} -> True ; _ -> False

instance Hashable a => Hashable (Marked a) 

instance Pretty a => Pretty ( Marked a) where
   pretty m = case m of
       Original a -> pretty a
       Marked a -> pretty a <> "#"
       Auxiliary a -> pretty a

mark_top :: Term v a -> Term v (Marked a)
mark_top  (Node f args) = 
          Node (Marked f) $ map (fmap Original) args

defined s = S.fromList $ do 
                u <- rules s
                let Node f args = lhs u
                -- will raise exception if lhs is variable
                return f

-- | compute the DP transformed system.

dp :: (Ord v, Ord s) 
   => RS s (Term v s) 
   -> RS (Marked s) (Term v (Marked s))
dp s = 
   let os = map ( \ u -> Rule { relation = Weak
                               , lhs = fmap Original $ lhs u  
                               , rhs = fmap Original $ rhs u  
                               , top = False
                               } )
           $ rules s
       def = defined s
       us = do 
            u <- rules s
            let ssubs = S.fromList $ strict_subterms $ lhs u
                walk r = if S.member r ssubs then [] else case r of
                    -- will raise exception if rhs contains 
                    -- a variable that is not in lhs
                    Node f args -> 
                        ( if S.member f def then (r :) else id )
                        ( args >>= walk )
            r <- walk $ rhs u
            return $ Rule { relation = Strict
                          , lhs = mark_top $ lhs u
                          , rhs = mark_top r 
                          , top = True
                          }
   in RS { signature = map Marked ( S.toList def )
                     ++ map Original ( signature s )
         , rules = us ++ os
         , separate = separate s 
         } 
