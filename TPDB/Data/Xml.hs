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
    toContents ( Node f args ) = rmkel "funapp" 
            $ rmkel "name" ( toContents f )
           ++ map ( \ arg -> mkel "arg" $ toContents arg ) args




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



