{-# language Arrows, NoMonomorphismRestriction, PatternSignatures #-}

-- | construct data object from XML tree.

module TPDB.XTC.Read where

-- implementations follows these examples:
-- http://www.haskell.org/haskellwiki/HXT/Practical/

import TPDB.Data
import TPDB.Data.Attributes

import Text.XML.HXT.Arrow.XmlArrow

import Text.XML.HXT.Arrow.XmlState ( runX )
import Text.XML.HXT.Arrow.ReadDocument ( readString )
import Text.XML.HXT.Arrow.XmlOptions ( a_validate )
import Text.XML.HXT.DOM.XmlKeywords (v_0)
import Control.Arrow
import Control.Arrow.ArrowList
import Control.Arrow.ArrowTree

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C
import Text.XML.HXT.IO.GetFILE
import Text.XML.HXT.Arrow.ReadDocument

atTag tag = deep (isElem >>> hasName tag)

getTerm = getVar <+> getFunApp

getVar = proc x -> do
    nm <- getText <<< getChildren <<< hasName "var" -< x
    returnA -< Var $ mk 0 nm

getFunApp = proc x -> do
    sub <- hasName "funapp" -< x
    nm <- getText <<< gotoChild "name" -< sub
    gs <- listA ( getTerm <<< gotoChild "arg" ) -< sub
    let c = mk (length gs) nm
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
    sig <- getSignature <<<  getChild "trs" -< x
    returnA -< Problem { trs = rs
                        , TPDB.Data.strategy = st
                        , TPDB.Data.full_signature = sig
                        , type_ = ty
                        , startterm = case stt of
                             [] -> Nothing
                             [x] -> x
                        , attributes = compute_attributes $ rules rs
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
    sig <- getSignature -< x
    str <- getRules Strict <<< getChild "rules" -< x
    nostr <- listA ( getRules Weak <<< getChild "relrules" <<< getChild "rules" ) -< x
    -- FIXME: check that symbols are use with correct arity
    returnA -< RS { signature = case sig of
                       Signature fs -> do f <- fs ; return $ mk (fs_arity f) (fs_name f)
                       HigherOrderSignature {} -> []
                  , rules = str ++ concat nostr
                  , separate = False -- for TRS, don't need comma between rules
                  }

getSignature =
      ( getFOSignature <<< getChild "signature" )
  <+> ( getHOSignature <<< getChild "higherOrderSignature" )

getFOSignature = proc x -> do
    fs <- listA ( getFuncsym <<< getChild "funcsym" ) -< x
    returnA -< Signature fs

getHOSignature = proc x -> do
    returnA -< HigherOrderSignature

getFuncsym = proc x -> do
    nm <- getText <<< gotoChild "name" -< x
    ar <- getRead <<< gotoChild "arity" -< x
    th <- listA ( getRead <<< gotoChild "theory" ) -< x
    rm <- listA ( listA (getRead <<< gotoChild "entry") <<< gotoChild "replacementmap" ) -< x
    returnA -< Funcsym  { fs_name = nm
                        , fs_arity = ar
                        , fs_theory = case th of [] -> Nothing ; [t] -> Just t
                        , fs_replacementmap = case rm of [] -> Nothing ; [r] -> Just (Replacementmap r)
                        }

getRead = proc x -> do s <- getText -< x ; returnA -< read s

getRules str = proc x -> do
    returnA <<< listA ( getRule str  <<< getChild "rule" ) -< x

getRule str = proc x -> do
    l <-  getTerm <<< isElem <<< gotoChild "lhs" -< x
    r <-  getTerm <<< isElem <<< gotoChild "rhs" -< x
    returnA -< Rule { lhs = l, relation = str, rhs = r, top = False }

readProblems :: FilePath -> IO [ Problem Identifier Identifier ]
readProblems file =
   runX ( readDocument [] file >>> getProblem )

readProblemsBS :: BS.ByteString -> IO [ Problem Identifier Identifier ]
readProblemsBS s =
   runX ( readString [] (C.unpack s) >>> getProblem )
