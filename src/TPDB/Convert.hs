{-# language OverloadedStrings #-}

module TPDB.Convert where

import TPDB.Data
import Control.Monad ( forM, guard )

srs2trs :: SRS Identifier -> TRS Identifier Identifier
srs2trs s = s { separate = False
              , rules = map convert_srs_rule $ rules s
              , signature = map (set_arity 1) $ signature s
              }

set_arity a s = s { arity = a }

convert_srs_rule u =
    let v = mk 0 "x"
        handle = unspine v . map (set_arity 1)
    in  u { lhs = handle $ lhs u
          , rhs = handle $ rhs u
          }

trs2srs :: TermC v s => TRS v s -> Maybe ( SRS s )
trs2srs t = do
    us <- forM ( rules t ) convert_trs_rule
    return $ t { separate = True , rules = us }

convert_trs_rule u = do
      ( left_spine, left_base ) <- spine $ lhs u
      ( right_spine, right_base ) <- spine $ rhs u
      guard $ left_base == right_base
      return $ u { lhs = left_spine, rhs = right_spine }

unspine :: TermC v s => v -> [s] -> Term v s
unspine v = foldr (  \ c t -> Node c [ t ] ) ( Var v )

-- | success iff term consists of unary symbols
-- and the lowest node is a variable
spine :: TermC v s => Term v s -> Maybe ( [s], v )
spine t = case t of
    Node f args -> do
      [ arg ] <- return args
      ( sp, base ) <- spine arg
      return ( f : sp, base )
    Var v -> return ( [] , v )
