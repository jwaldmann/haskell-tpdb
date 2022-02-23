{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module TPDB.CPF.Proof.Util where

import qualified Data.Map as M
import           Data.List (nub)
import           TPDB.Data 
import           TPDB.CPF.Proof.Type hiding (name)
import           TPDB.DP 
import Data.String (fromString)
import Data.Hashable

fromMarkedIdentifier :: Marked Identifier -> Symbol
fromMarkedIdentifier = \case 
  Original i -> SymName i
  Marked i   -> SymSharp $ SymName i

sortVariables :: (Ord s, Hashable s) => Rule (Term Identifier s) -> Rule (Term Identifier s)
sortVariables r = r { lhs = vmap mapVar $ lhs r
                    , rhs = vmap mapVar $ rhs r
                    }
  where
    oldVars      = nub $ voccs $ lhs r
    newVars      = zipWith mkNewVar [1..] oldVars
    mkNewVar i v = v { name = fromString $ "x" ++ show i }
    mapping      = M.fromList $ zip oldVars newVars
    mapVar v     = case M.lookup v mapping of
      Just v' -> v'
      Nothing -> error "TPDB.CPF.Proof.Util.sortVariables"
