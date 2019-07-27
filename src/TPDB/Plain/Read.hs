-- | textual input,
-- cf. <http://www.lri.fr/~marche/tpdb/format.html>

{-# language PatternSignatures, TypeSynonymInstances, FlexibleInstances #-}

module TPDB.Plain.Read where

import TPDB.Data

import Text.Parsec (parse)

import qualified Data.Text.Lazy as T

import Text.Parsec.Text.Lazy as P
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.Char
import Control.Applicative
-- import Text.Parsec.Combinator
import Data.Functor.Identity (Identity)

import TPDB.Pretty (pretty)
import TPDB.Plain.Write ()
import Data.Text ()
import Data.String (fromString)

import Control.Monad ( guard, void )
import Data.List ( nub )

trs :: T.Text -> Either String ( TRS Identifier Identifier )
trs s = case Text.Parsec.parse reader "input" s of
    Left err -> Left $ show err
    Right t  -> Right t

srs :: T.Text -> Either String ( SRS Identifier )
srs s = case Text.Parsec.parse reader "input" s of
    Left err -> Left $ show err
    Right t  -> Right t

-- type Parser = Parsec ByteString () 

class Reader a where reader :: P.Parser a

-- | warning: by definition, {}[] may appear in identifiers
lexer :: GenTokenParser T.Text () Identity
lexer = makeTokenParser
    $ LanguageDef
       { nestedComments = True
       , opStart        = oneOf ":!#$%&*+./<=>?@\\^|-~"
       , opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
       , reservedOpNames = []
       , caseSensitive  = True
       , identStart  = alphaNum <|> oneOf "_:!#$%&*+./<=>?@\\^|-~{}[]'"
       , identLetter = alphaNum <|> oneOf "_:!#$%&*+./<=>?@\\^|-~{}[]'"
       , commentLine = "" , commentStart = "" , commentEnd = ""
       , reservedNames = [ "VAR", "THEORY", "STRATEGY", "RULES", "->", "->=" ]
       }

instance Reader Identifier where 
    reader = do
        i :: String <- identifier lexer 
        return $ mk 0 $ fromString i

instance Reader s =>  Reader [s] where
    reader = many reader

-- NOTE: this is dangerous since we read the variables as constants,
-- and this needs to be patched up later.
-- NOTE: this is more dangerous as we do not set the arity of identifiers
instance ( Reader v ) => Reader ( Term v Identifier ) where
    reader = do
        f  <- reader 
        xs <- ( parens lexer $ commaSep lexer reader ) <|> return []
        return $ Node ( f { arity = Prelude.length xs } ) xs

instance Reader u => Reader ( Rule u ) where
    reader = do
        l <- reader
        rel <-  do reservedOp lexer "->"  ; return Strict
          <|> do reservedOp lexer "->=" ; return Weak
          -- FIXME: for the moment, we do not parse this
          -- as it would deviate from published TPDB syntax
          -- <|> do reservedOp lexer "=" ; return Equal
        r <- reader
        return $ Rule { lhs = l, relation = rel, top = False, rhs = r }

data Declaration u
     = Var_Declaration [ Identifier ]
     | Theory_Declaration 
     | Strategy_Declaration 
     | Rules_Declaration [ Rule u ]
     | Unknown_Declaration
       -- ^ this is super-ugly: a parenthesized, possibly nested, 
       -- possibly comma-separated, list of identifiers or strings

declaration :: Reader u => Bool -> P.Parser ( Declaration u )
declaration sep = parens lexer $ 
           do reserved lexer "VAR" ; xs <- many reader 
              return $ Var_Declaration xs
       <|> do reserved lexer "THEORY" 
              error "TPDB.Plain.Read: parser for THEORY decl. missing"
       <|> do reserved lexer "STRATEGY" 
              error "TPDB.Plain.Read: parser for THEORY decl. missing"
       <|> do reserved lexer "RULES" 
              us <- if sep then do 
                        many $ do 
                            u <- reader ; optional $ comma lexer
                            return u
                        -- yes, TPDB contains some trailing commas, e.g., z008
                        -- ( RULES a b -> b a , )
                    else many reader
              return $ Rules_Declaration us
       <|> do anylist ; return Unknown_Declaration

anylist = void 
        $ commaSep lexer 
        $ many ( void ( identifier lexer ) <|> parens lexer anylist )

instance Reader ( SRS Identifier ) where
    reader = do 
        many space
        ds <- many $ declaration True
        return $ make_srs ds

instance Reader ( TRS Identifier Identifier ) where
    reader = do
        many space
        ds <- many $ declaration False
        return $ make_trs ds

repair_signature_srs sys = 
    let sig = nub $ do u <- rules sys ; lhs u ++ rhs u
    in  sys { signature = sig }

make_srs :: Eq s => [ Declaration [s] ] -> SRS s
make_srs ds = repair_signature_srs $
    let us = do Rules_Declaration us <- ds ; us        
    in  RS { rules = us, separate = True }

repair_signature_trs sys = 
    let sig = nub $ do u <- rules sys ; lsyms ( lhs u ) ++ lsyms ( rhs u)
    in  sys { signature = sig }

make_trs :: [ Declaration ( Term Identifier Identifier ) ] 
         -> TRS Identifier Identifier
make_trs ds = repair_signature_trs $
    let vs = do Var_Declaration vs <- ds ; vs
        us = do Rules_Declaration us <- ds ; us
        us' = repair_variables vs us
    in  RS { rules = us', separate = False }


repair_variables vars rules = do
    let xform ( Node c [] ) | c `elem` vars = Var c
        xform ( Node c args ) = Node c ( map xform args )
    rule <- rules  
    return $ rule { lhs = xform $ lhs rule
                  , rhs = xform $ rhs rule
                  }

