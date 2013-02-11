module TPDB.Convert where

import TPDB.Data
import Control.Monad ( forM, guard )

srs2trs :: SRS s -> TRS Identifier s
srs2trs s = s { separate = False
              , rules = map convert_srs_rule $ rules s
              }  

convert_srs_rule u = 
    let v = mk 0 "x" 
    in  u { lhs = unspine v $ lhs u
          , rhs = unspine v $ rhs u        
          } 
                  
trs2srs :: Eq v => TRS v s -> Maybe ( SRS s )
trs2srs t = do
    us <- forM ( rules t ) convert_trs_rule 
    return $ t { separate = True , rules = us }

convert_trs_rule u = do
      ( left_spine, left_base ) <- spine $ lhs u
      ( right_spine, right_base ) <- spine $ rhs u
      guard $ left_base == right_base     
      return $ u { lhs = left_spine, rhs = right_spine }

unspine :: v -> [s] -> Term v s
unspine v = foldr (  \ c t -> Node c [ t ] ) ( Var v )

-- | success iff term consists of unary symbols
-- and the lowest node is a variable
spine :: Term v s -> Maybe ( [s], v )
spine t = case t of
    Node f args -> do
      [ arg ] <- return args
      ( sp, base ) <- spine arg 
      return ( f : sp, base )
    Var v -> return ( [] , v ) 
