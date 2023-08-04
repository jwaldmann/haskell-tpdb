{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}

module TPDB.XTC.Write

( document
, writeFile, renderLBS, renderText, def
)

where

import qualified TPDB.Data as D
import qualified Data.Map as M

import qualified Data.Text as T
import qualified Text.XML as X
import Prelude hiding (writeFile)
import Text.XML (writeFile,renderLBS,renderText)
import Text.Hamlet.XML
import Data.Default (def)

document :: D.Problem D.Identifier D.Identifier -> X.Document
document p = X.Document (X.Prologue [] Nothing []) root [] where
    root = X.Element "problem"
      (M.fromList [("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance")
                  ,("type","termination")
                  ,("xsi:noNamespaceSchemaLocation","xtc.xsd")
                  ])
      [xml|
<trs>^{trs $ D.trs p}
$maybe s <- D.strategy p
  <strategy>^{strategy s}
|]

strategy s = case s of
  D.Full -> [xml|FULL|]

trs :: D.TRS D.Identifier D.Identifier -> [X.Node]
trs rs = [xml|
<rules>
  $forall u <- D.strict_rules rs
    ^{rule u}
  $if not (null (D.weak_rules rs))
    <relrules>
      $forall u <- D.weak_rules rs
        ^{rule u}
<signature>
  $forall f <- D.signature rs
    <funcsym>
      <name>#{T.pack $ show f}
      <arity>#{T.pack $ show $ D.arity f}
|]

rule (l,r) = [xml|
<rule>
  <lhs>^{term l}
  <rhs>^{term r}
|]
  
term :: D.Term D.Identifier D.Identifier -> [X.Node]
term t = case t of
  D.Var v -> [xml|
<var>#{T.pack $ show v}
|]
  D.Node f args -> [xml|
<funapp>
  <name>#{T.pack $ show f}
  $forall arg <- args
     <arg>^{term arg}
|]
