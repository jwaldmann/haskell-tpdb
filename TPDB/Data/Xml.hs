{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}

module TPDB.Data.Xml where

import TPDB.Data
import TPDB.Xml

import Text.XML.HaXml.Types (QName (..) )
import Text.XML.HaXml.XmlContent.Haskell hiding ( element, many )

import Data.Typeable

-- | FIXME: move to separate module
instance XmlContent Identifier where
    parseContents = do
        CString _ s _ <- next 
        return  $ mknullary s
    toContents i =
          -- probably not here: E.xmlEscape E.stdXmlEscaper
          -- this introduces whitespace between &lt; and =
          -- [ CString False $ show i ]
          -- and this creates a CDATA element
          -- [ CString True $ show i ]
          -- so here comes an UGLY HACK:
          [ CString False ( escape $ show i ) () ]


instance ( Typeable ( Term v c ) , XmlContent v, XmlContent c )
         => XmlContent ( Term v c ) where
    toContents ( Var v ) = rmkel "var" $ toContents v
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

    toContents ( Node f args ) = rmkel "funapp" 
            $ no_sharp_name_HACK ( toContents f )
           ++ map ( \ arg -> mkel "arg" $ toContents arg ) args

no_sharp_name_HACK e = e

sharp_name_HACK e = case e of
    [ CElem ( Elem (N "sharp") [] cs ) () ] -> 
        rmkel "sharp" $ rmkel "name" cs
    _ -> rmkel "name" e




instance HTypeable ( Rule ( Term v c )) where
     toHType _ = Prim "Rule" "Rule"

instance ( HTypeable ( Rule ( Term v c) )
         , XmlContent ( Term v c ) ) 
         => XmlContent ( Rule ( Term v c ) ) where
    toContents u =
        return $ mkel "rule" 
               [ mkel "lhs" $ toContents $ lhs u
               , mkel "rhs" $ toContents $ rhs u
               ]



