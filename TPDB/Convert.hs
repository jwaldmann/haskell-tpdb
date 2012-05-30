module TPDB.Convert where

import TPDB.Data
import Control.Monad ( forM, guard )

srs2trs :: SRS s -> TRS Identifier s
srs2trs s = s { separate = False
              , rules = map convert_srs_rule $ rules s
              }  

convert_srs_rule u = 
    let v = Var ( Identifier "x" 0 )
        build = foldr ( \ c t -> Node c [ t ] ) v
    in  u { lhs = build $ lhs u
          , rhs = build $ rhs u        
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

spine t = case t of
    Node f [ a ] -> do
      ( sp, base ) <- spine a 
      return ( f : sp, base )
    Var v -> return ( [] , v ) 
