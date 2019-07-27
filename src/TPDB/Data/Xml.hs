{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}
{-# language QuasiQuotes #-}

module TPDB.Data.Xml where

import TPDB.Data
import TPDB.Xml

import Text.Hamlet.XML
import Data.String
import Data.Typeable

-- | FIXME: move to separate module
instance XmlContent Identifier where
    parseContents = content &| \ c -> mknullary c
    toContents i =
          -- probably not here: E.xmlEscape E.stdXmlEscaper
          -- this introduces whitespace between &lt; and =
          -- [ CString False $ show i ]
          -- and this creates a CDATA element
          -- [ CString True $ show i ]
          -- so here comes an UGLY HACK:
          [xml|#{fromString $ escape $ show i}|]


instance (  Show v, XmlContent v, XmlContent c )
         => XmlContent ( Term v c ) where
    toContents ( Var v ) = [xml|<var>#{fromString $ show v}|]
{-
-- for Rainbow:
    toContents ( Node f xs ) = return $ mkel "app"
         $ mkel "fun" ( toContents f )
         : map ( \ x -> mkel "arg" $ toContents x ) xs
-}
-- for CPF:

-- the problem is this:
-- a variable is an Identifier, and must look like "<var>foo</var>"
-- a constructor symbol is also an Identifier
-- but it must look like "<funapp><name>bar<name>..."
-- so it should be wrapped into <name>
-- but no, if the constructor is sharped (or labelled)
-- then it must look like "<funapp><sharp><name>bar..."
-- price question: what is the correct 
-- instance XmlContent Identifier ?
-- the answer probably is "there is none",
-- and toContents (Term v Identifier) should never occur,
-- instead need to call  toContents (Term v Symbol)

    toContents ( Node f args ) =
      [xml|<funapp>
             ^{no_sharp_name_HACK ( toContents f )}
             $forall arg <- args
               <arg>^{toContents arg}
      |]


no_sharp_name_HACK e = e

{-
sharp_name_HACK e = case e of
    [ CElem ( Elem (N "sharp") [] cs ) () ] -> 
        rmkel "sharp" $ rmkel "name" cs
    _ -> rmkel "name" e

-}



instance ( XmlContent ( Term v c ) ) 
         => XmlContent ( Rule ( Term v c ) ) where
    toContents u =
      [xml|<rule>
             <lhs>^{toContents $ lhs u}
             <rhs>^{toContents $ rhs u}
      |]




