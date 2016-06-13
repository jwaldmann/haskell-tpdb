{-# language OverloadedStrings #-}

module TPDB.Data.Attributes where

import TPDB.Data.Term
import TPDB.Data.Rule
import TPDB.Pretty

import qualified Data.Set as S
import qualified Data.Map.Strict as M

data Attributes = Attributes
  { size_of_signature :: Int
  , max_arity :: Int
  , total_term_size :: Int
  , max_term_size :: Int
  , max_term_depth :: Int
  , left_linear :: Bool
  , right_linear :: Bool
  , linear :: Bool
  , max_var_count :: Int
  , max_var_depth :: Int
  }

instance Pretty Attributes where
  pretty a = "Attributes" <+> braces ( fsep $ punctuate comma
       [ "size_of_signature =" <+> pretty (size_of_signature a)
       , "max_arity =" <+> pretty (max_arity a)
       , "total_term_size =" <+> pretty (total_term_size a)
       , "max_term_size =" <+> pretty (max_term_size a)
       , "max_term_depth =" <+> pretty (max_term_depth a)
       , "left_linear =" <+> pretty (left_linear a)
       , "right_linear =" <+> pretty (right_linear a)
       , "linear =" <+> pretty (linear a)
       , "max_var_count =" <+> pretty (max_var_count a)
       , "max_var_depth =" <+> pretty (max_var_depth a)
       ] )
  

compute_attributes
  :: (Ord v, Ord c)
  => [Rule (Term v c)] -> Attributes
compute_attributes us =
  let terms = do u <- us; [lhs u, rhs u]
      sterms = terms >>= subterms
      sig = S.fromList ( terms >>= symsl )
      term_sizes = map size terms
      term_depths = map depth terms
      vcs = map varcount us
  in Attributes
     { size_of_signature = S.size sig
     , max_arity = maximum $ do
       u <- us ; t <- [lhs u, rhs u]
       Node f args <- subterms t
       return $ length args
     , total_term_size = sum term_sizes
     , max_term_size = maximum term_sizes
     , max_term_depth = maximum term_depths
     , left_linear = and $ do vc <- vcs ; (k,(l,r)) <- M.toList vc ; return $ l <= 1
     , right_linear = and $ do vc <- vcs ; (k,(l,r)) <- M.toList vc ; return $ r <= 1
     , linear = and $ do vc <- vcs ; (k,(l,r)) <- M.toList vc ; return $ l == 1 && r == 1 -- FIXME: or (l == r) ?
     , max_var_count = maximum $ map M.size vcs
     , max_var_depth = maximum $ map length $ terms >>= varpos
     }

varcount :: Ord v => Rule (Term v c) -> M.Map v (Int,Int)
varcount u = M.mergeWithKey ( \ k l r -> Just (l,r)) ( M.map ( \k -> (k,0))) (M.map ( \k -> (0,k)))
        (varcount_term $ lhs u) (varcount_term $ rhs u)

varcount_term :: Ord v => Term v c -> M.Map v Int
varcount_term t = M.fromListWith (+) $ do
  (p, Var v) <- positions t
  return (v, 1)
  
