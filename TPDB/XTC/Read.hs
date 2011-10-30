{-# language Arrows, NoMonomorphismRestriction, PatternSignatures #-}

-- | construct data object from XML tree
-- implementations follows these examples:
-- http://www.haskell.org/haskellwiki/HXT/Practical/

module TPDB.XTC.Read where

import TPDB.XTC.Term
import TPDB.XTC.Identifier
import TPDB.XTC.Problem
import TPDB.XTC.RS
import TPDB.XTC.Rule


import Text.XML.HXT.Arrow.XmlArrow

import Text.XML.HXT.Arrow.XmlState ( runX )
import Text.XML.HXT.Arrow.ReadDocument ( readString )
import Text.XML.HXT.Arrow.XmlOptions ( a_validate )
import Text.XML.HXT.DOM.XmlKeywords (v_0)
import Control.Arrow
import Control.Arrow.ArrowList
import Control.Arrow.ArrowTree

import Data.Set

atTag tag = deep (isElem >>> hasName tag)

getProblem = atTag "problem" >>> proc x -> do
    ty <- getType <<< getAttrValue "type" -< x
    rs <- getTRS <<< getChild "trs" -< x
    st <- getStrategy <<< getChild "strategy" -< x
    returnA -< case st of
        Full -> Problem { trs = rs
                        , TPDB.XTC.Problem.strategy = st
                        , type_ = ty 
                        }
        _    -> error $ unwords [ "cannot handle strategy", show st ]

getType = proc x -> do
    returnA -< case x of
        "termination" -> Termination
        "complexity" -> Complexity

getStrategy = proc x -> do
    cs <- getText <<< getChildren -< x
    returnA -< case cs of
        "FULL" -> Full

getTRS = proc x -> do
    sig <- getSignature <<< getChild "signature" -< x
    str <- getRules True <<< getChild "rules" -< x
    nostr <- listA ( getRules False <<< getChild "relrules" <<< getChild "rules" ) -< x
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
    returnA -< Identifier { arity = read ar, name = nm }

getRules str = proc x -> do
    returnA <<< listA ( getRule str  <<< getChild "rule" ) -< x

getRule str = proc x -> do
    l <-  getTerm <<< isElem <<< gotoChild "lhs" -< x
    r <-  getTerm <<< isElem <<< gotoChild "rhs" -< x
    returnA -< Rule { lhs = l, strict = str, rhs = r, top = False }

readProblems :: FilePath -> IO [ Problem Identifier Identifier ]
readProblems file = do
    cs <- readFile file
    runX ( readString [] cs >>> getProblem )



