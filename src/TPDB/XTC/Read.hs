-- | construct data object from XML tree.

{-# language NoMonomorphismRestriction, PatternSignatures, OverloadedStrings #-}


module TPDB.XTC.Read (readProblemF, readProblemT ) where

import TPDB.Data
import TPDB.Data.Attributes

import Text.XML
import Text.XML.Cursor
import qualified Data.Text as ST
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import Data.String
import Control.Monad.Catch


readProblemF :: FilePath -> IO ( Problem Identifier Identifier )
readProblemF file = do
  doc <- Text.XML.readFile Text.XML.def file
  case getProblem $ fromDocument doc of
    [] -> error "input contains no XTC problem"
    [p] -> return p
    ps -> error "input contains more than one XTC problem"

readProblemT :: LT.Text -> Either SomeException (Problem Identifier Identifier)
readProblemT t = do
  [p] <- ( getProblem . fromDocument ) <$> Text.XML.parseText Text.XML.def t
  return p


getProblem = element "problem" >=> \ c -> do
    let ! ty = case c $| attribute "type" of
         [ "termination" ] -> Termination
         [ "complexity"  ] -> Complexity
         _ -> error "type"
    let ! st = case c $/ element "strategy" &/ content of
         [ "FULL" ]      -> Just Full
         [ "INNERMOST" ] -> Just Innermost
         [ "OUTERMOST" ] -> Just Outermost
         [] -> Nothing
         _ -> error "strategy"
    rs <- c $/ element "trs" >=> getTRS         
    let stt = c $/ element "startterm" &/ getStartterm
    sig <- c $/ element "trs" >=> getSignature 
    return $ Problem { trs = rs
                        , TPDB.Data.strategy = st
                        , TPDB.Data.full_signature = sig
                        , type_ = ty
                        , startterm = case stt of
                             [] -> Nothing
                             [x] -> Just x
                        , attributes = compute_attributes $ rules rs
                        }

getTerm :: Cursor -> [ Term Identifier Identifier ]
getTerm = getVar <> getFunApp

getVar :: Cursor -> [ Term Identifier Identifier ]
getVar = element "var" &/ \ c -> ( Var . mk 0 ) <$> content c

getFunApp :: Cursor -> [ Term Identifier Identifier ]
getFunApp = element "funapp" >=> \ c -> do
  nm <- c $/ element "name" &/ content
  let args = c $/ element "arg" &/ getTerm
      f = mk (length args) $ nm
  return $ Node f args
          

        

getStartterm =
     (element "constructor-based" &| const  Startterm_Constructor_based )
  <> (element "full" &| const Startterm_Full   ) 

getTRS c = do
    sig <- getSignature c
    str <- c $/ element "rules" >=> getRulesWith Strict 
    let nostr = ( c $/ element "relrules" >=> getRulesWith Weak )
    -- FIXME: check that symbols are use with correct arity
    return $ RS { signature = case sig of
                       Signature fs -> do f <- fs ; return $ mk (fs_arity f) ( fs_name f)
                       HigherOrderSignature {} -> []
                  , rules = str ++ concat nostr
                  , separate = False -- for TRS, don't need comma between rules
                  }

getSignature c =  ( c $/ element "signature" >=> getFOSignature  )
  <> ( c $/ element "higherOrderSignature" &| const HigherOrderSignature )

getFOSignature c =
  return $ Signature ( c $/ element "funcsym" >=> getFuncsym )

getFuncsym c = do
    nm <- c $/ element "name" &/ content
    ar <- c $/ element "arity" &/ read_content
    let th = c $/ element "theory" &/ read_content
        rm = c $/ element "replacementmap" >=> \ c ->
          return $ c $/ element "entry" &/ read_content
    return $ Funcsym  { fs_name = nm
                        , fs_arity = ar
                        , fs_theory = case th of [] -> Nothing ; [t] -> Just t
                        , fs_replacementmap = case rm of [] -> Nothing ; [r] -> Just (Replacementmap r)
                        }

read_content c = (read . ST.unpack) <$> content c

getRulesWith s =  element "rules" >=> \ c ->
  return ( c $/ ( element "rule" >=> getRule s ) )

getRule :: Relation -> Cursor -> [ Rule (Term Identifier Identifier) ]
getRule s c = 
  ( \ l r -> Rule {lhs=l,relation=s,rhs=r,top=False})
    <$> (c $/ element "lhs" &/ getTerm) <*> (c $/ element "rhs" &/ getTerm)

