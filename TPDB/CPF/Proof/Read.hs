{-# language Arrows, NoMonomorphismRestriction, PatternSignatures #-}

module TPDB.CPF.Proof.Read where

import TPDB.CPF.Proof.Type 
import TPDB.Data

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

{- | dangerous: 
not all constructor arguments will be set.
the function produces something like

      CertificationProblem { input = CertificationProblemInput 
                          , proof = TrsTerminationProof undefined
                          }  
-}

readCP :: String -> IO [ CertificationProblem ]
readCP s = runX ( X.withTraceLevel 0 $ readString [] s >>> getCP )

getCP = atTag "certificationProblem" >>> proc x -> do
    inp <- getInput <<< getChild "input" -< x
    pro <- getProof <<< getChild "proof" -< x
    ver <- getText <<< gotoChild "cpfVersion" -< x
    returnA -< CertificationProblem { input = inp, proof = pro, cpfVersion = "2.2" }

getInput = getTerminationInput <+> getComplexityInput

getTerminationInput = atTag "input" >>> proc x -> do
    trsI <- getTrsInput <<< getChild "trsInput" -< x    
    returnA -< TrsInput $ RS { rules = trsI, separate = False }

getComplexityInput = atTag "input" >>> proc x -> do
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

getTrsInput = proc x -> do
    sys <- getTrs <<< getChild "trs" -< x
    rels <- listA ( getTrsWith Weak <<< getChild "relativeRules" ) -< x
    returnA -< sys ++ concat rels

getTrs = getTrsWith Strict

getTrsWith s = proc x -> do
    str <- getRules s <<< getChild "rules" -< x
    returnA -< str

getProof = getDummy "trsTerminationProof" ( TrsTerminationProof undefined )
       <+> getDummy "trsNonTerminationProof" ( TrsNonterminationProof undefined )
       <+> getDummy "relativeTerminationProof" ( RelativeTerminationProof undefined )
       <+> getDummy "relativeNonTerminationProof" ( RelativeNonterminationProof undefined )
       <+> getDummy "complexityProof" ( ComplexityProof undefined )

getDummy t c = atTag t >>> proc x -> do
    returnA -< c 

getRules str = proc x -> do
    returnA <<< listA ( getRule str  <<< getChild "rule" ) -< x

getRule str = proc x -> do
    l <-  getTerm <<< isElem <<< gotoChild "lhs" -< x
    r <-  getTerm <<< isElem <<< gotoChild "rhs" -< x
    returnA -< Rule { lhs = l, relation = str, rhs = r, top = False }


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

atTag tag = deep (isElem >>> hasName tag)

