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
import qualified Data.Text as DT
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Control.Monad.Catch

import TPDB.Pretty

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
    [] -> error "input contains no certification problem"
    [cp] -> return cp
    cps -> error $ unlines $
      ( "input contains " ++ show (length cps) ++ " certification problems" )
      : map (show . pretty . trsinput_trs . input ) cps

element1 name c =
  let info = take 100 $ show c
  in  case element name c of
        [] -> error $ "missing element " <> show name <> " in " <> info
        [e] -> [e]
        _ -> error $ "more than one element " <> show name <> " in " <> info


fromDoc :: Cursor -> [ CertificationProblem ]
fromDoc = element1 "certificationProblem" >=> \ c -> 
  ( CertificationProblem
     <$> (c $/ element "input" &/ getInput )
     <*> (c $/ element "cpfVersion" &/ content )
     <*> (c $/ element "proof" &/ getProof)
     <*> (c $/ element "origin" >=> return [ignoredOrigin] )
  )

getInput =  getTerminationInput
   <> getComplexityInput
   <> getACTerminationInput

getTerminationInput c = c $| element "trsInput" &/ getTrsInput &| 
   \ i -> TrsInput $ RS { rules = i , separate = False }   

getACTerminationInput = element "acRewriteSystem" >=> \ c -> do
    let as = c $/ element "Asymbols" &/ getSymbol
        cs = c $/ element "Csymbols" &/ getSymbol
    acrs <- getTrsInput c
    return $ ACRewriteSystem
      { trsinput_trs = RS { rules = acrs, separate = False }
      , asymbols = as
      , csymbols = cs
      }

getSymbol = element1 "name" &/ \ c -> mk 0 <$> content c 

getComplexityInput = element "input" >=> \ c -> do
    trsI <- c $/ element "complexityInput" &/ element "trsInput" &/ getTrsInput
    cm <- c $/ getComplexityMeasure 
    cc <- c $/ getComplexityClass 
    return $ ComplexityInput
        { trsinput_trs = RS { rules = trsI, separate = False }
        , complexityMeasure = cm
        , complexityClass = cc
        }

getComplexityMeasure = 
        getDummy "derivationalComplexity" DerivationalComplexity
    <>  getDummy "runtimeComplexity" RuntimeComplexity

getComplexityClass = element "polynomial" &/ \ c ->
  ( \ s -> ComplexityClassPolynomial { degree = read $ DT.unpack s } ) <$> content c


getTrsInput c =
     ( c $/ element "trs" &/  getRulesWith Strict )
  <> ( c $/ element "relativeRules" &/ getRulesWith Weak )


getRulesWith s =  element1 "rules" >=> \ c ->
  return ( c $/ ( element "rule" >=> getRule s ) )

getRule :: Relation -> Cursor -> [ Rule (Term Identifier Identifier) ]
getRule s c = 
  ( \ l r -> Rule {lhs=l,relation=s,rhs=r,top=False})
    <$> (c $/ element "lhs" &/ getTerm) <*> (c $/ element "rhs" &/ getTerm)

getProof :: Cursor -> [ Proof ]
getProof c = c $|
     (    getDummy "trsTerminationProof" ( TrsTerminationProof undefined )
       <> getDummy "trsNonterminationProof" ( TrsNonterminationProof undefined )
       <> getDummy "relativeTerminationProof" ( RelativeTerminationProof undefined )
       <> getDummy "relativeNonterminationProof" ( RelativeNonterminationProof undefined )
       <> getDummy "complexityProof" ( ComplexityProof undefined )
       <> getDummy "acTerminationProof" ( ACTerminationProof undefined )
     )

getDummy :: X.Name -> b -> Cursor -> [ b ]
getDummy t c cursor = cursor $| element t >=> return [ c]

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
          
