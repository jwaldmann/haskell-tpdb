{-# language Arrows, NoMonomorphismRestriction, PatternSignatures #-}

-- | construct data object from XML tree.

module TPDB.XTC.Read where

-- implementations follows these examples:
-- http://www.haskell.org/haskellwiki/HXT/Practical/

import TPDB.Data

import Text.XML.HXT.Arrow.XmlArrow

import Text.XML.HXT.Arrow.XmlState ( runX )
import Text.XML.HXT.Arrow.ReadDocument ( readString )
import Text.XML.HXT.Arrow.XmlOptions ( a_validate )
import Text.XML.HXT.DOM.XmlKeywords (v_0)
import Control.Arrow
import Control.Arrow.ArrowList
import Control.Arrow.ArrowTree

atTag tag = deep (isElem >>> hasName tag)

getTerm = getVar <+> getFunApp

getVar = proc x -> do
    nm <- getText <<< getChildren <<< hasName "var" -< x
    returnA -< Var $ Identifier { name = nm, arity = 0 }

getFunApp = proc x -> do
    sub <- hasName "funapp" -< x
    nm <- getText <<< gotoChild "name" -< sub
    gs <- listA ( getTerm <<< gotoChild "arg" ) -< sub
    let c = Identifier { name = nm, arity = length gs }
    returnA -< Node c gs
          
gotoChild tag = proc x -> do
    returnA <<< getChildren <<< getChild tag -< x

getChild tag = proc x -> do
    returnA <<< hasName tag <<< isElem <<< getChildren -< x

getProblem = atTag "problem" >>> proc x -> do
    ty <- getType <<< getAttrValue "type" -< x
    rs <- getTRS <<< getChild "trs" -< x
    st <- getStrategy <<< getChild "strategy" -< x
    stt <- listA ( getStartterm <<< getChild "startterm" ) -< x
    returnA -< Problem { trs = rs
                        , TPDB.Data.strategy = st
                        , type_ = ty 
                        , startterm = case stt of
                             [] -> Nothing
                             [x] -> x
                        }

getType = proc x -> do
    returnA -< case x of
        "termination" -> Termination
        "complexity" -> Complexity

getStrategy = proc x -> do
    cs <- getText <<< getChildren -< x
    returnA -< case cs of
        "FULL" -> Just Full

getStartterm = ( proc x -> do
        getChild "constructor-based" -< x
        returnA -< Just Startterm_Constructor_based
   ) <+>  ( proc x -> do
        getChild "full" -< x
        returnA -< Just Startterm_Full
   ) <+> ( proc x -> do returnA -< Nothing )

getTRS = proc x -> do
    sig <- getSignature <<< getChild "signature" -< x
    str <- getRules Strict <<< getChild "rules" -< x
    nostr <- listA ( getRules Weak <<< getChild "relrules" <<< getChild "rules" ) -< x
    -- FIXME: check that symbols are use with correct arity
    th <- listA ( atTag "theory" ) -< x
    returnA -< case th of
        [] -> RS { signature = sig
                  , rules = str ++ concat nostr
                  , separate = False -- for TRS, don't need comma between rules
                  }
        _  -> error $ unwords [ "cannot handle theories" ]

getSignature = proc x -> do
    returnA <<< listA ( getFuncsym <<< getChild "funcsym" ) -< x

getFuncsym = proc x -> do
    nm <- getText <<< gotoChild "name" -< x
    ar <- getText <<< gotoChild "arity" -< x
    returnA -< Identifier { name = nm , arity = read ar }

getRules str = proc x -> do
    returnA <<< listA ( getRule str  <<< getChild "rule" ) -< x

getRule str = proc x -> do
    l <-  getTerm <<< isElem <<< gotoChild "lhs" -< x
    r <-  getTerm <<< isElem <<< gotoChild "rhs" -< x
    returnA -< Rule { lhs = l, relation = str, rhs = r, top = False }

readProblems :: FilePath -> IO [ Problem Identifier Identifier ]
readProblems file = do
    cs <- readFile file
    runX ( readString [] cs >>> getProblem )



