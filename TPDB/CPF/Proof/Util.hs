{-# LANGUAGE LambdaCase #-}
module TPDB.CPF.Proof.Util where

import TPDB.Data (Identifier)
import TPDB.CPF.Proof.Type (Symbol (..))
import TPDB.DP (Marked (..))

fromMarkedIdentifier :: Marked Identifier -> Symbol
fromMarkedIdentifier = \case 
  Original i -> SymName i
  Marked i   -> SymSharp $ SymName i
