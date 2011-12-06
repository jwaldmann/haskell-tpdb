{-# OPTIONS -fglasgow-exts #-}

-- | internal representation of Rainbow termination proofs,
-- see <http://color.loria.fr/>
-- this file is modelled after rainbow/proof.ml
-- it omits constructors not needed for matrix interpretations (for the moment)
module TPDB.Rainbow.Proof.Type 

where

import TPDB.Data hiding ( Type (..))
import TPDB.Pretty
import TPDB.Plain.Write () -- just instances
import Text.PrettyPrint.HughesPJ

import Text.XML.HaXml.XmlContent.Haskell hiding ( text )
import qualified Text.Parsec as P

import qualified Data.Time as T
import Data.Typeable

data Vector a = Vector [ a ] 
   deriving Typeable

data Matrix a = Matrix [ Vector a ]
   deriving Typeable

data MaxPlus = MinusInfinite | Finite Integer
   deriving ( Show, Read, Typeable )

data Mi_Fun a = 
     Mi_Fun { mi_const :: Vector a
            , mi_args :: [ Matrix a ]
            }
   deriving Typeable

data Matrix_Int = forall k a 
    . ( XmlContent k, XmlContent a, Typeable a ) -- Haskell2Xml a 
     => 
     Matrix_Int { mi_domain :: Domain
                , mi_dim :: Integer
		, mi_duration :: T.NominalDiffTime -- ^ this is an extension
  	        , mi_start :: T.UTCTime
	        , mi_end :: T.UTCTime
                , mi_int :: [ (k , Mi_Fun a ) ]
                }
   deriving Typeable

data Domain = Natural | Arctic | Arctic_Below_Zero | Tropical 
    deriving ( Show, Eq, Ord, Typeable )

instance Pretty Domain where pretty = text . show
instance Pretty T.NominalDiffTime where pretty = text . show
instance Pretty T.UTCTime where pretty = text . show

data Red_Ord 
    = Red_Ord_Matrix_Int Matrix_Int
    | Red_Ord_Simple_Projection Simple_Projection
    | Red_Ord_Usable_Rules Usable_Rules
   deriving Typeable

data Usable_Rules = Usable_Rules [ Identifier ]
    deriving Typeable

instance Pretty Usable_Rules where 
    pretty (Usable_Rules sp) = text "Usable_Rules" <+> pretty sp


data Simple_Projection = Simple_Projection [ ( Identifier, Int ) ]
    deriving Typeable

instance Pretty Simple_Projection where 
    pretty (Simple_Projection sp) = text "Simple_Projection" <+> pretty sp

data Claim =
     Claim { system :: TRS Identifier Identifier
           , property :: Property
           }
   deriving Typeable

data Proof =
     Proof { claim :: Claim
           , reason :: Reason
           }
   deriving Typeable

data Property 
     = Termination 
     | Top_Termination 
     | Complexity ( Function, Function )
   deriving ( Typeable )

instance Show Property where show = render . pretty

instance Pretty Property where
    pretty p = case p of
        Termination -> text "YES # Termination"
        Top_Termination -> text "YES # Top_Termination"
        Complexity ( lo, hi ) -> text "YES" <+> pretty ( lo, hi ) 

-- | see specification:
-- http://termination-portal.org/wiki/Complexity
data Function
    = Unknown
    | Polynomial { degree :: Maybe Int }
    | Exponential
   deriving ( Typeable )

instance Show Function where show = render . pretty

instance Pretty Function where
    pretty f = case f of
        Unknown -> text "?"
        Polynomial { degree = Nothing } -> 
            text "POLY"
        Polynomial { degree = Just 0 } -> 
            text "O(1)"
        Polynomial { degree = Just d } -> 
            text "O" <+> parens ( text "n^" <> pretty d)



data Reason 
    = Trivial
    | MannaNess Red_Ord Proof
    | MarkSymb Proof
    | DP Proof
    | Reverse Proof
    | As_TRS Proof 
    | As_SRS Proof 
    | SCC [ Proof ] -- ^ proposed extension
    | RFC Proof -- ^ experimental (not in Rainbow)
    | Undo_RFC Proof -- ^ experimental (not in Rainbow)
    | Bounded_Matrix_Interpretation Proof -- ^ TODO add more info
   deriving Typeable

data Over_Graph = HDE | HDE_Marked 
    deriving ( Show, Typeable )

data Marked a = Hd_Mark a | Int_Mark a 
    deriving ( Eq, Ord, Typeable )






