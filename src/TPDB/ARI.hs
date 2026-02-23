{-# language OverloadedStrings #-}
{-   # options_ghc -fdefer-typed-holes #-}

module TPDB.ARI where

import Data.AttoLisp
import qualified Data.Attoparsec.ByteString as DAB
import Data.Attoparsec.Number
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.HashMap.Strict as M
import Control.Applicative (many)
import Control.Monad (guard)
import TPDB.Data
import TPDB.Plain.Write
import TPDB.Pretty

-- writer
put :: TRS Identifier Identifier -> BSL.ByteString
put = BSL.concat . map encodeLn . w

encodeLn x = encode x `BSL.snoc` lf
  where lf = fromIntegral $ fromEnum '\n' -- HACK

w :: TRS Identifier Identifier -> [Lisp]
w sys =
  let sym s = Symbol $ name s
      sig = do
        s <- signature sys
        return $ List [ Symbol "fun", sym s
                 , Number $ I $ fromIntegral $ arity s
                      ]
      trm t = case t of
        Var v -> sym v
        Node f args -> List $ sym f : map trm args
      rul = do
         u <- rules sys
         return $ List
               $ [ Symbol "rule", trm (lhs u), trm (rhs u) ]
               <> case relation u of
                   Strict -> []
                   Weak -> [ Symbol ":cost", Number (I 0) ]
  in  [ List [Symbol "format", Symbol "TRS" ] ]
      <> sig
      <> rul


-- parser

get :: BS.ByteString -> Either String (TRS Identifier Identifier)
get s = DAB.parseOnly ( p <* DAB.atEnd ) s

p :: DAB.Parser (TRS Identifier Identifier)
p = do
  cl : auses <- many lisp
  guard $ cl == List [Symbol "format", Symbol "TRS"]
  let symbolize s0 = maybe s0 id $ do
        ('|', s1) <- T.uncons s0; (s2, '|') <- T.unsnoc s1; return s2
      funs = do
        List [Symbol "fun", Symbol s, Number (I a)] <- auses
        return (s, mk (fromIntegral a) $ symbolize s)
      sig = M.fromListWith (error "conflict") funs
  let rs :: [ Rule (Term Identifier Identifier) ]
      rs = do
        List (Symbol "rule" : l : r : trailer) <- auses
        return $ Rule { lhs = termof sig l
                      , rhs = termof sig r
                      , relation = case trailer of
                          [ ] -> Strict
                          [ Symbol ":cost", Number (I 0) ]
                             -> Weak
                      , top = False
                      , original_variable = Nothing
                      }
  return $ RS
    { signature = map snd funs
    , rules = rs
    , separate = False
    }

termof :: M.HashMap T.Text Identifier -> Lisp
  -> Term Identifier Identifier
termof sig (List (Symbol s : args)) =
  case M.lookup s sig of
    Just f | arity f == length args ->
             Node f $ map (termof sig) args
termof sig (Symbol s) = case M.lookup s sig of
  Nothing -> Var $ mk 0 s
  Just f -> Node f [] -- ?
        
                           
                          
