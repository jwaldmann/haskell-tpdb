{-# language Arrows, NoMonomorphismRestriction, PatternSignatures, OverloadedStrings, LambdaCase #-}

module TPDB.CPF.Proof.Read where

import TPDB.CPF.Proof.Type 
import TPDB.Data

{-
import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.Arrow.XmlState ( runX )
import Text.XML.HXT.Arrow.ReadDocument ( readString )
import Text.XML.HXT.Arrow.XmlOptions ( a_validate )
import Text.XML.HXT.DOM.XmlKeywords (v_0)

import Control.Arrow
import Control.Arrow.ArrowList
import Control.Arrow.ArrowTree

import qualified TPDB.CPF.Proof.Write as W -- for testing
import qualified Text.XML.HXT.Arrow.XmlState as X 

-}

import qualified Text.XML as X
import Text.XML.Cursor
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Control.Monad.Catch

{- | dangerous: 
not all constructor arguments will be set.
the function produces something like

      CertificationProblem { input = CertificationProblemInput 
                          , proof = TrsTerminationProof undefined
                          }  
-}

readCP :: T.Text -> Either SomeException [CertificationProblem]
readCP t = ( fromDoc . fromDocument ) <$> X.parseText X.def t

readFile :: FilePath -> IO CertificationProblem
readFile f = do
  doc  <- X.readFile X.def f
  case fromDoc $ fromDocument doc of
    [cp] -> return cp
    [] -> error "input contains no certification problem"
    _ -> error "input contains more than one certification problems"

fromDoc :: Cursor -> [ CertificationProblem ]
fromDoc c = c $| element "certificationProblem" &/ \ c -> 
  ( CertificationProblem
     <$> (c $/ element "input" &/ getInput )
     <*> (c $/ element "cpfVersion" &/ content )
     <*> (c $/ element "proof" &/ getProof)
     <*> (c $/ element "origin" &/ return [ignoredOrigin] )
  )

getInput = getTerminationInput
   --  <> getComplexityInput
   <> getACTerminationInput

getTerminationInput =  element "trsInput" &/ getTrsInput &| 
   \ i -> TrsInput $ RS { rules = i , separate = False }   

getACTerminationInput c =
  c $/ element "acRewriteSystem" &/ \ c -> do
    acrs <- getTrsInput c
    let as = c $/ element "Asymbols" &/ getSymbol
        cs = c $/ element "Csymbols" &/ getSymbol
    return $ ACRewriteSystem
      { trsinput_trs = RS { rules = acrs, separate = False }
      , asymbols = as
      , csymbols = cs
      }

getSymbol = element "name" &/ \ c -> mk 0 <$> content c

{-

getComplexityInput = hasName "input" >>> proc x -> do
    y <- getChild "complexityInput" -< x
    trsI <- getTrsInput <<< getChild "trsInput" -< y
    cm <- getComplexityMeasure -< y
    cc <- getComplexityClass -< y
    returnA -< ComplexityInput
        { trsinput_trs = RS { rules = trsI, separate = False }
        , complexityMeasure = cm
        , complexityClass = cc
        }

getComplexityMeasure = 
        getDummy "derivationalComplexity" DerivationalComplexity
    <+> getDummy "runtimeComplexity" RuntimeComplexity

getComplexityClass = proc x -> do
    d <- getText <<< gotoChild "polynomial" -< x
    returnA -< ComplexityClassPolynomial { degree = read d }

-}

getTrsInput c = c $/ element "trs" &/ \ c ->
       getTrsWith Strict c
       -- <> ( element "relativeRules" c $/ getTrsWith Weak )

getTrs = getTrsWith Strict

getTrsWith s = element "rules" &/ getRules s

getRules :: Relation -> Cursor -> [[ Rule (Term Identifier Identifier) ]]
getRules s c = return ( c $/ element "rule" &/ getRule s )

getRule :: Relation -> Cursor -> [ Rule (Term Identifier Identifier) ]
getRule s c =
  ( \ l r -> Rule {lhs=l,relation=s,rhs=r,top=False})
    <$> (c $/ element "lhs" &/ getTerm) <*> (c $/ element "rhs" &/ getTerm)

getProof :: Cursor -> [ Proof ]
getProof c = c $/
     (    getDummy "trsTerminationProof" ( TrsTerminationProof undefined )
       <> getDummy "trsNonterminationProof" ( TrsNonterminationProof undefined )
       <> getDummy "relativeTerminationProof" ( RelativeTerminationProof undefined )
       <> getDummy "relativeNonterminationProof" ( RelativeNonterminationProof undefined )
       <> getDummy "complexityProof" ( ComplexityProof undefined )
       <> getDummy "acTerminationProof" ( ACTerminationProof undefined )
     )

getDummy :: X.Name -> b -> Cursor -> [ b ]
getDummy t c cursor = cursor $/ element t &/ return [ c]

getTerm :: Cursor -> [ Term Identifier Identifier ]
getTerm c = getVar c <> getFunApp c

getVar :: Cursor -> [ Term Identifier Identifier ]
getVar = element "var" &/ \ c -> ( Var . mk 0 ) <$> content c

getFunApp :: Cursor -> [ Term Identifier Identifier ]
getFunApp = element "funapp" &/ \ c -> do
  nm <- c $/ element "name" &/ content
  let args = c $/ element "arg" &/ getTerm
      f = mk (length args) $ nm
  return $ Node f args
          
