{-# language TypeSynonymInstances, FlexibleContexts, FlexibleInstances, UndecidableInstances, OverlappingInstances, IncoherentInstances, PatternSignatures, DeriveDataTypeable, OverloadedStrings, LambdaCase #-}

{-# OPTIONS_GHC -Wincomplete-patterns #-}

-- | from internal representation to XML, and back

module TPDB.CPF.Proof.Write where

import TPDB.CPF.Proof.Type
import qualified TPDB.Data as T

import TPDB.Xml 
import Text.XML
import TPDB.Data.Xml 

import Data.List ( nub )
import Data.Char ( toLower )
import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.Text as T
import qualified Data.Time as T
import Control.Monad
import Data.Typeable
import Data.Ratio

tox :: CertificationProblem -> Document 
tox p = 
    let style = Instruction
          "xml-stylesheet" "type=\"text/xsl\" href=\"cpfHTML.xsl\""
        pro = Prologue [ MiscInstruction style ] Nothing []
        [ NodeElement e ] = toContents p
    in  Document pro e []

instance XmlContent CertificationProblem where
   parseContents = error "parseContents not implemented"

   toContents cp = rmkel "certificationProblem"
         [ mkel "input" $ toContents ( input cp )
         , mkel "cpfVersion" [ nospaceString $ cpfVersion cp ]
         , mkel "proof" $ toContents ( proof cp )
         , mkel "origin" $ toContents ( origin cp )
         ]

instance XmlContent Origin where
   parseContents = error "parseContents not implemented"

   toContents o = case o of
       ProofOrigin t -> rmkel "proofOrigin" $ toContents t

instance XmlContent Tool where
   parseContents = error "parseContents not implemented"

   toContents t = rmkel "tool" 
         [ mkel "name"    [ nospaceString $ name    t ]
         , mkel "version" [ nospaceString $ version t ]
         ]

instance XmlContent CertificationProblemInput where
   parseContents = error "parseContents not implemented"

   toContents i = case i of
      TrsInput {} -> rmkel "trsInput" $ toContents ( symbolize $ trsinput_trs i )
      ComplexityInput {} -> rmkel "complexityInput" $ concat
          [ rmkel "trsInput" $ toContents $ symbolize $ trsinput_trs i
          ]

instance XmlContent ( T.TRS Identifier Symbol ) where
   parseContents = error "parseContents not implemented"

   toContents s = rmkel "trs" 
       $ rmkel "rules" $ concat $ map toContents $ T.rules s

instance ( Typeable t, XmlContent t  ) 
        => XmlContent ( T.Rule t) where
   parseContents = error "parseContents not implemented"

   toContents u = rmkel "rule" $ concat
      [ rmkel "lhs" ( toContents $ T.lhs u )
      , rmkel "rhs" ( toContents $ T.rhs u )
      ]

instance XmlContent Proof where
   parseContents = error "parseContents not implemented"

   toContents p = 
     let missing t = rmkel t $ rmkel "missing-toContents-instance" [] 
     in  case p of
       TrsTerminationProof p -> toContents p
       TrsNonterminationProof p -> toContents p
       RelativeTerminationProof p -> missing "RelativeTerminationProof"
       RelativeNonterminationProof p -> missing "RelativeNonterminationProof"
       ComplexityProof p -> missing "ComplexityProof"

instance XmlContent DPS where
   parseContents = error "parseContents not implemented"

   toContents ( DPS rules ) = rmkel "dps" 
        $ rmkel "rules" $ rules >>= toContents

instance XmlContent TrsTerminationProof where
   parseContents = error "parseContents not implemented"

   toContents p = rmkel "trsTerminationProof" $ case p of
      RIsEmpty -> rmkel "rIsEmpty" []
      DpTrans {} -> rmkel "dpTrans" $ concat
          [ toContents $ dptrans_dps p
          , rmkel "markedSymbols" [ nospaceString "true" ]
          , toContents $ dptrans_dpProof p
          ]
      StringReversal {} -> rmkel "stringReversal" $ concat
          [ toContents $ symbolize $ trs p
          , toContents $ trsTerminationProof p
          ]
      RuleRemoval {} -> rmkel "ruleRemoval" $ concat
          [ toContents $ rr_orderingConstraintProof p
          , toContents $ symbolize $ trs p
          , toContents $ trsTerminationProof p
          ]
      Bounds {} -> rmkel "bounds" $ concat
          [ toContents $ symbolize $ trs p
          , toContents $ bounds_type p
          , rmkel "bound" $ toContents $ bounds_bound p 
          , rmkel "finalStates" $ concat
             $ map toContents $ bounds_finalStates p
          , toContents $ bounds_closedTreeAutomaton p
          ]

symbolize trs = 
    ( fmap (T.tmap SymName) trs )
    { T.signature = map SymName $ T.signature trs }

instance XmlContent Bounds_Type where
  toContents t = case t of
    Roof -> rmkel "roof" []
    Match -> rmkel "match" []

instance XmlContent State where
  toContents (State s) = rmkel "state"  $ toContents s

instance XmlContent ClosedTreeAutomaton where
  toContents c = concat
    [ toContents $ cta_treeAutomaton c
    , toContents $ cta_criterion c
    ]

instance XmlContent Criterion where
  toContents c = case c of
    Compatibility -> rmkel "compatibility" []

instance XmlContent TreeAutomaton where
  toContents a = rmkel "treeAutomaton" $ concat
    [ rmkel "finalStates" $ concat
       $ map toContents $ ta_finalStates a
    , rmkel "transitions" $ concat
       $ map toContents $ ta_transitions a
    ]

instance XmlContent Transition where
  toContents t = rmkel "transition" $ concat
    [ rmkel "lhs" $ toContents $ transition_lhs t
    , rmkel "rhs" $ concat
       $ map toContents $ transition_rhs t
    ]

instance XmlContent Transition_Lhs where
  toContents s = case s of
    Transition_Symbol {} -> concat
      [ toContents $ tr_symbol s
      , rmkel "height" $ toContents $ tr_height s
      , concat $ map toContents $ tr_arguments s
      ]
    Transition_Epsilon s -> toContents s

instance XmlContent Model where
  parseContents = error "parseContents not implemented"

  toContents model = rmkel "model" $ case model of
    FiniteModel carrierSize interprets ->
      rmkel "finiteModel" $ concat
        [ rmkel "carrierSize"  $ toContents carrierSize
        , concatMap toContents interprets
        ]

instance XmlContent DpProof where
  parseContents = error "parseContents not implemented"

  toContents p = rmkel "dpProof" $ case p of
    PIsEmpty -> rmkel "pIsEmpty" []
    RedPairProc {} -> case rppUsableRules p of
      Nothing -> rmkel "redPairProc" $ concat
        [ toContents $ rppOrderingConstraintProof p
        , toContents $ rppDps p
        , toContents $ rppDpProof p
        ]
      Just (DPS ur) -> rmkel "redPairUrProc" $ concat
        [ toContents $ rppOrderingConstraintProof p
        , toContents $ rppDps p
        , rmkel "usableRules" $ rmkel "rules" $ concatMap toContents ur
        , toContents $ rppDpProof p
        ]
    DepGraphProc cs -> rmkel "depGraphProc" $ concat $ map toContents cs

    SemLabProc {} -> rmkel "semlabProc" $ concat
      [ toContents $ slpModel p
      , toContents $ slpDps p
      , case slpTrs p of
          DPS rules -> rmkel "trs" $ rmkel "rules" $ rules >>= toContents

      , toContents $ slpDpProof p
      ]

    UnlabProc {} -> rmkel "unlabProc" $ concat
      [ toContents $ ulpDps p
      , case ulpTrs p of
          DPS rules -> rmkel "trs" $ rmkel "rules" $ rules >>= toContents
      , toContents $ ulpDpProof p
      ]

instance XmlContent DepGraphComponent where
    toContents dgc = rmkel "component" $ concat $
        [ {- rmkel "dps" $ -} toContents $ dgcDps dgc
        , rmkel "realScc" 
           --  $ toContents $ dgcRealScc dgc
           -- NO, Bool is encoded as text, not as attribute
            $ toContents $ dgcRealScc dgc 
        ] ++ 
        [ {- rmkel "dpProof" $ -} toContents $ dgcDpProof dgc
        | dgcRealScc dgc
        ]

instance XmlContent OrderingConstraintProof where
  parseContents = error "parseContents not implemented"

  toContents (OCPRedPair rp) = rmkel "orderingConstraintProof" 
                             $ toContents rp
           
instance XmlContent RedPair where
  parseContents = error "parseContents not implemented"

  toContents rp = rmkel "redPair" $ case rp of
    RPInterpretation i -> toContents i
    RPPathOrder      o -> toContents o

instance XmlContent Interpretation where
   parseContents = error "parseContents not implemented"

   toContents i = rmkel "interpretation" $
        rmkel "type" ( toContents $ interpretation_type i )
     ++ concatMap toContents ( interprets i )
      
instance XmlContent Interpretation_Type where
   parseContents = error "parseContents not implemented"

   toContents t = rmkel "matrixInterpretation" $ concat 
      [ toContents ( domain t )
      , rmkel "dimension"       $ toContents $ dimension t 
      , rmkel "strictDimension" $ toContents $ strictDimension t
      ]
     
instance XmlContent Domain where
   parseContents = error "parseContents not implemented"

   toContents d = rmkel "domain" $ case d of
       Naturals -> rmkel "naturals" []
       Rationals delta -> rmkel "rationals" 
         $ rmkel "delta" $ toContents delta
       Arctic d -> rmkel "arctic" $ toContents d
       Tropical d -> rmkel  "tropical" $ toContents d

instance XmlContent Rational where
    parseContents = error "parseContents not implemented"

    toContents r = rmkel "rational" 
        [ mkel "numerator"   $ toContents $ numerator   r 
        , mkel "denominator" $ toContents $ denominator r 
        ]

instance XmlContent Interpret  where
   parseContents = error "parseContents not implemented"

   toContents i = rmkel "interpret" $ concat
                    [ toContents $ symbol i
                    , rmkel "arity" $ toContents $ arity i 
                    , toContents $ value i
                    ]

instance XmlContent Value where
   parseContents = error "parseContents not implemented"

   toContents v = case v of
      Polynomial p -> toContents p
      ArithFunction f -> toContents f

instance XmlContent Polynomial where
   parseContents = error "parseContents not implemented"

   toContents p = rmkel "polynomial" $ case p of
       Sum     ps -> rmkel "sum"     $ concat ( map toContents ps )
       Product ps -> rmkel "product" $ concat ( map toContents ps )
       Polynomial_Coefficient c -> rmkel "coefficient" $ toContents c
       Polynomial_Variable v -> rmkel "variable" [ nospaceString v ]

instance XmlContent ArithFunction where
  parseContents = error "parseContents not implemented"

  toContents af = rmkel "arithFunction" $ case af of
    AFNatural  n      -> rmkel "natural"  $ toContents n 
    AFVariable n      -> rmkel "variable" $ toContents n 
    AFSum     afs     -> rmkel "sum"      $ concatMap toContents afs
    AFProduct afs     -> rmkel "product"  $ concatMap toContents afs
    AFMin     afs     -> rmkel "min"      $ concatMap toContents afs
    AFMax     afs     -> rmkel "max"      $ concatMap toContents afs
    AFIfEqual a b t f -> rmkel "ifEqual"  $ concatMap toContents [a,b,t,f]

instance XmlContent Coefficient where
   parseContents = error "parseContents not implemented"

   toContents v = case v of
       Matrix vs -> rmkel "matrix" $ concat ( map toContents vs )
       Vector cs -> rmkel "vector" $ concat ( map toContents cs )
       Coefficient_Coefficient i -> 
          rmkel "coefficient" $ toContents i

instance XmlContent Exotic where
    parseContents = error "parseContents not implemented"

    toContents e = case e of
       Minus_Infinite -> rmkel "minusInfinity" []
       E_Integer i -> rmkel "integer" $ toContents i
       Plus_Infinite -> rmkel "plusInfinity" []

-- see remark in TPDB.Data.Xml (sharp_name_HACK)

instance XmlContent Symbol where
  parseContents = error "parseContents not implemented"

  toContents (SymName id) = rmkel "name" $ toContents id
  toContents (SymSharp sym) = rmkel "sharp" $ toContents sym
  toContents (SymLabel sym label) = rmkel "labeledSymbol" 
                                  $ toContents sym ++ (toContents label)

instance XmlContent Label where
  parseContents = error "parseContents not implemented"

  toContents (LblNumber is) = 
    rmkel "numberLabel" $ map (\i -> mkel "number" $ toContents i ) is

  toContents (LblSymbol ss) = rmkel "symbolLabel" $ concatMap toContents ss

instance XmlContent PathOrder where
  parseContents = error "parseContents not implemented"

  toContents (PathOrder ps as) = rmkel "pathOrder" $ concat
    [ rmkel "statusPrecedence" $ concatMap toContents ps
    , if null as then []
      else rmkel "argumentFilter" $ concatMap toContents as
    ]

instance XmlContent PrecedenceEntry where
  parseContents = error "parseContents not implemented"

  toContents (PrecedenceEntry s a p) = rmkel "statusPrecedenceEntry" $ concat
    [ toContents s
    , rmkel "arity"      $ toContents a
    , rmkel "precedence" $ toContents p
    , rmkel "lex"        [ ]
    ]

instance XmlContent ArgumentFilterEntry where
  parseContents = error "parseContents not implemented"

  toContents (ArgumentFilterEntry s a f) = rmkel "argumentFilterEntry" $ concat
    [ toContents s
    , rmkel "arity" $ toContents a
    , case f of 
        Left i   -> rmkel "collapsing" $ toContents i 
        Right is -> rmkel "nonCollapsing" 
                  $ map (\i -> mkel "position" $ toContents i) is
    ]

instance XmlContent TrsNonterminationProof where
  toContents tnp = rmkel "trsNonterminationProof" $ case tnp of
    VariableConditionViolated -> rmkel "variableConditionViolated" []
    TNP_RuleRemoval sys sub -> rmkel "ruleRemoval"
      $ concat [ toContents $ symbolize sys, toContents sub ]
    TNP_StringReversal sys sub -> rmkel "stringReversal"
      $ concat [ toContents $ symbolize sys , toContents sub ]
    Loop {rewriteSequence = rs, substitution = sub, context = ctx } -> rmkel "loop"
        $ concat  [ toContents rs, toContents sub, toContents ctx ]

instance XmlContent RewriteSequence where
  toContents (RewriteSequence start steps) =
    rmkel "rewriteSequence" $ concat
      [ rmkel "startTerm" $ toContents start 
      , concatMap toContents steps
      ]

instance XmlContent RewriteStep where
  toContents rs = rmkel "rewriteStep" $ concat
    [ rmkel "positionInTerm"
      $ concatMap (\ k -> rmkel "position" $ toContents k ) $ rs_position rs
    , toContents $ rs_rule rs
    , toContents $ rs_term rs
    ]

instance XmlContent Substitution where
  toContents (Substitution ses) = rmkel "substitution" $ concatMap toContents ses
instance XmlContent SubstEntry where
  toContents (SubstEntry v t) = rmkel "substEntry" $ concat
    [ toContents $ (T.Var v :: T.Term Identifier Symbol)
    , toContents $ t
    ]

instance XmlContent Context where
  toContents c = case c of
    Box -> rmkel "box" []
    FunContext {} -> rmkel "funContext" $ concat
      [ toContents $ fc_symbol c
      , rmkel "before" $ concatMap toContents $ fc_before c
      , toContents $ fc_here c
      , rmkel "after" $ concatMap toContents $ fc_after c
      ]
