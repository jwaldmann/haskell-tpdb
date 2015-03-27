{-# language TypeSynonymInstances, FlexibleContexts, FlexibleInstances, UndecidableInstances, OverlappingInstances, IncoherentInstances, PatternSignatures, DeriveDataTypeable #-}

-- | from internal representation to XML, and back

module TPDB.Rainbow.Proof.Xml where

import TPDB.Rainbow.Proof.Type

import qualified TPDB.CPF.Proof.Type as C

import TPDB.Xml
import TPDB.Data.Xml
-- import Matrix.MaxPlus ( MaxPlus )
-- import Autolib.Reader hiding ( many ) 
-- import Autolib.TES
-- import Autolib.TES.Identifier ( mk, mknullary, mkunary )

import TPDB.Data

import qualified Text.XML.HaXml.Escape as E
import qualified Text.XML.HaXml.Pretty as P

import Text.XML.HaXml.Types (QName (..) )
import Text.XML.HaXml.XmlContent.Haskell hiding ( element, many )
import Text.XML.HaXml.Types ( EncodingDecl(..), emptyST, XMLDecl(..) )

import Data.List ( nub )
import Data.Char ( toLower )
import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.Time as T
import Control.Monad
import Data.Typeable

tox :: Proof -> Document ()
tox p = 
    let xd = XMLDecl "1.0" ( Just $ EncodingDecl "ISO-8859-1" ) Nothing 
        pro = Prolog ( Just xd ) [] Nothing []
        t = toplevel p
        et =  E.xmlEscape E.stdXmlEscaper t
        -- don't escape, see remarks in Autolib.TES.Identifier
    in  Document pro emptyST t []


toplevel p = 
    let	atts = [ ( N "xmlns"
		 , AttValue [ Left "urn:rainbow.proof.format" ] 
		 )
	       , ( N "xmlns:xsi"
		 , AttValue [ Left "http://www.w3.org/2001/XMLSchema-instance" ]
		 )
	       , ( N "xsi:schemaLocation"
		 , AttValue [ Left "urn:rainbow.proof.format http://color.loria.fr/proof.xsd" ]
		 )
	       ]
        unProof [ CElem ( Elem (N "proof") [] cs ) _ ] = cs
    in  Elem (N "proof") atts $ unProof $ toContents p


instance ( Typeable a, XmlContent a ) => XmlContent ( Vector a ) where
    toContents ( Vector xs ) = 
        map ( \ x -> mkel "velem"  ( toContents x ) ) xs
    parseContents = wrap xread

instance XRead a => XRead ( Vector a ) where
    xread = fmap Vector $ many $ element "velem" xread


-- FIXME:
instance XmlContent MaxPlus where
    parseContents = do
        CString _ s _ <- next 
        return  $ read s
    toContents i =
          [ CString False ( escape $ show i ) () ]


-- | for some types, e.g. Integer
-- , do not use XmlContent instance for element type
-- but show them (as string).
-- reason: the XmlContent module contains an instance
-- for integer that produces <integer value="42"/>
-- and there is no way to turn this off.

data Xml_As_String a = Xml_As_String a
    deriving Typeable

instance ( Typeable a, Show a, Read a ) => XmlContent (Xml_As_String a) where
    toContents ( Xml_As_String x ) = [ CString False ( show x ) () ] 
    parseContents = wrap $ fmap Xml_As_String $ xfromstring

instance ( Typeable a, XmlContent a   )
	 => XmlContent ( Matrix a ) where
    toContents ( Matrix xs ) = 
        map ( \ x -> mkel "row"  ( toContents x ) ) xs
    parseContents = wrap xread

instance XRead a => XRead ( Matrix a ) where
    xread = do
        rs <- many $ element "row" xread 
	return $ Matrix rs

instance ( Typeable a, XmlContent a  )
	  => XmlContent ( Mi_Fun a ) where
    toContents ( Mi_Fun { mi_const = c, mi_args = xs } ) = return $
        mkel "mi_fun" 
              $ mkel "const"  ( toContents c )
	      : map  ( mkel "arg" . toContents ) xs
    parseContents = wrap xread

instance XRead a => XRead ( Mi_Fun a ) where
    xread = element "mi_fun" $ do
        con <- element "const" xread
	args <- many $ element "arg" xread
	return $ Mi_Fun { mi_const = con, mi_args = args }
        
instance C.ToExotic a => C.ToExotic (Xml_As_String a) where
    toExotic (Xml_As_String x) = C.toExotic x

--------------------------------------------------------------------------------

instance XmlContent Domain where
    toContents d = [ CString False ( show d ) ( )]

instance XmlContent Matrix_Int where
    toContents ( Interpretation { mi_domain = o, mi_dim = d, mi_int = i
			    , mi_start = s, mi_end = e, mi_duration = u } ) =
        return $ mkel ( case o of Natural -> "matrix_int" 
                                  Arctic -> "arctic_int" 
                                  Arctic_Below_Zero -> "arctic_bz_int" 
                                  Tropical -> "tropical_int" )
               [ mkel "dimension"  [ CString False ( show d ) () ] 
               , mkel "mi_map"  $ do
                   ( k, v ) <- i
                   return $ mkel "mapping" 
                          $ mkel "fun" ( toContents k )
			  : toContents v 
               -- the following are not standard rainbow,
               -- so they have to come after the standard elements
               -- because the rainbow parser then ignores them
	       , mkel "start" [ CString False ( show s ) () ] 
	       , mkel "end" [ CString False ( show e ) () ] 
	       , mkel "duration" [ CString False ( show u ) () ] 
               ]
    parseContents = wrap xread

instance XRead Matrix_Int where
    xread = foldr1 orelse 
	  [ mai "matrix_int" Natural ( undefined :: Xml_As_String Integer )
	  , mai "arctic_int" Arctic ( undefined :: MaxPlus )
	  , mai "arctic_bz_int" Arctic_Below_Zero ( undefined ::  MaxPlus )
	  ]

-- | FIXME this is broken because the keys could be
-- Identifier or Marked Identifier
mai :: forall a . ( Typeable a, XmlContent a, C.ToExotic a )
    => String -> Domain -> a -> CParser Matrix_Int
mai tag o v0 = element tag $ do
    d <- element "dimension" $ xfromstring
    i <- element "mi_map" $ many $ element "mapping" $ do
          k :: Identifier <- element "fun" $ xread -- FIXME
	  v :: Mi_Fun a <- xread
	  return ( k,  v )
    s <- element "start" $ xfromstring
    e <- element "end" $ xfromstring
    let u = T.diffUTCTime e s
    return $ Interpretation
	   { mi_domain = o, mi_dim = d, mi_int = i
	   , mi_start = s, mi_end = e, mi_duration = u
	   }

instance XmlContent Red_Ord where
    toContents ro = case ro of
         Red_Ord_Matrix_Int mi -> return $
             mkel "order"  $ toContents mi
         Red_Ord_Simple_Projection sp -> return $
             mkel "order"  $ toContents sp
    parseContents = wrap xread

instance XmlContent Simple_Projection where
    toContents ( Simple_Projection sp ) = return $
         mkel "simple_projection" $ map ( \ (f,i) ->
               mkel "project" [ mkel "symbol" $ toContents (Hd_Mark $ unP f)
                              , mkel "position" $ toContents $ Xml_As_String i 
                              ]
               ) sp
    parseContents = wrap xread

instance XRead Simple_Projection where
    xread = element "simple_projection" 
          $ fmap Simple_Projection xread

instance XRead Red_Ord where
    xread = element "order" 
          $ orelse ( fmap Red_Ord_Matrix_Int xread )
                   ( fmap Red_Ord_Simple_Projection xread )

instance HTypeable Proof where
    toHType p = Prim "proof" "proof" 


instance XmlContent Proof where
    toContents p0 = let { p = reason p0 } in return $ mkel "proof" $ case p of
        Trivial -> return $ mkel "trivial"  []
        MannaNess o p -> return 
            $ mkel "manna_ness"  $ toContents o ++ toContents p 
        MarkSymb p -> return $ mkel "mark_symbols" $ toContents p 
        DP p -> return $ mkel "dp" $ toContents p 
        As_TRS p -> return $ mkel "as_trs" $ toContents p
        Reverse p -> return $ mkel "reverse" $ toContents p
	SCC parts -> return $ mkel "scc_decomp"
	    $ mkel "graph" ( toContents HDE_Marked )
	    : do
	        p <- parts
	        let sys = system $ claim p 
		    vs = signature sys
		    u = head $ filter strict $ rules sys
		return $ mkel "scc"
		       $ toContents ( externalize vs u )
		       ++ toContents p
    parseContents = wrap xread

instance XRead Proof where
    xread = element "proof" $ do
        let cl = undefined
	rs <- foldr1 orelse
	    [ element "trivial" $ return Trivial
	    , element "reverse" $ fmap Reverse xread
	    , element "as_trs" $ fmap As_TRS xread
	    , element "manna_ness" $ do 
                      return () ; o <- xread ; p <- xread ; return $ MannaNess o p 
	    , element "mark_symbols" $ fmap MarkSymb xread
	    , element "dp" $ fmap DP xread
	    , complain "Proof"
	    ]
        return $ Proof { claim = cl, reason = rs }

externalize :: [ Identifier ]
	    -> Rule ( Term Identifier Identifier )
	    -> Rule ( Term Identifier ( Marked Identifier ) )
externalize vs u = 
    let -- HACK: rainbow/DPSCC requires variables be numbered
	-- according to inverse order in declaration (!?)
	m  = Map.fromList 
	   $ zip ( reverse vs ) 
	   $ map ( \ i -> mknullary $ "v" ++ show i )  [ 1 .. ]
        rename :: Identifier -> Identifier
	rename v = let Just w = Map.lookup v m in w
	handle ( Node f args ) = Node ( Hd_Mark $ unP f ) 
	    $ map ( fmap Int_Mark . vmap rename ) args
    in  Rule { lhs = handle $ lhs u, rhs = handle $ rhs u
             , relation = Strict , top = False } 

-- | super ugly risky: name mangling
unP :: Identifier -> Identifier
unP k = let cs = show k 
        in  case last cs of 'P' -> mknullary $ init cs

instance XmlContent Over_Graph where
    toContents og = return $ mkel ( map toLower $ show og ) []

instance ( Typeable a, XmlContent a ) => XmlContent ( Marked a ) where
    toContents m = case m of
{-
        Hd_Mark x -> return $ mkel "hd_mark" $ toContents x
        Int_Mark x -> return $ mkel "int_mark" $ toContents x
-}
-- HACK
        Hd_Mark x -> rmkel "sharp" $ rmkel "name" $ toContents x
        Int_Mark x -> rmkel "name" $ toContents x

    parseContents = wrap xread

instance ( Typeable a, XmlContent a ) => XRead ( Marked a ) where
    xread = foldr1 orelse
	  [ fmap Hd_Mark  $ element "hd_mark" xread
	  , fmap Int_Mark $ element "int_mark" xread
	  , complain "Marked"
	  ]

-- FIXME
instance XRead Identifier where
    xread = CParser $ \ ( c : cs ) -> 
        return ( mknullary "some_identifier" , cs )
	-- error $ info [c]

-- FIXME: we will need this for SCC
-- instance XmlContent TES where
    
