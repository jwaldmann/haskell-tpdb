{-# language Arrows, NoMonomorphismRestriction, PatternSignatures #-}

module TPDB.XTC.Term where



import Text.XML.HXT.Arrow.XmlArrow hiding ( getName )
import Control.Arrow
import Control.Arrow.ArrowList
import Control.Arrow.ArrowTree


getTerm = getVar <+> getFunApp

getVar = proc x -> do
    nm <- getText <<< getChildren <<< hasName "var" -< x
    returnA -< Var $ Identifier { arity = 0, name = nm }

getFunApp = proc x -> do
    sub <- hasName "funapp" -< x
    nm <- getText <<< gotoChild "name" -< sub
    gs <- listA ( getTerm <<< gotoChild "arg" ) -< sub
    let c = Identifier { arity = length gs , name = nm }
    returnA -< Node c gs
          
gotoChild tag = proc x -> do
    returnA <<< getChildren <<< getChild tag -< x

getChild tag = proc x -> do
    returnA <<< hasName tag <<< isElem <<< getChildren -< x
