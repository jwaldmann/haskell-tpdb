{-# language NoMonomorphismRestriction #-}

module TPDB.Pretty

( Doc, Pretty (..)
, render, renderWide, renderCompact, renderPretty
, displayIO
, fsep, sep, hsep, vsep, vcat, hcat
, parens, brackets, angles, braces, enclose
, punctuate, comma, nest, list, tupled
  , module Data.Monoid, empty
, text
, (<+>), ($$)
)

where

import Data.Text.Prettyprint.Doc
  ( Doc, Pretty(..), comma
  , punctuate,align, parens, braces, angles, brackets, nest, enclose
  , list, tupled
  )
import qualified Data.Text.Prettyprint.Doc as D
import qualified Data.Text.Prettyprint.Doc.Render.Text as T

import Data.String ( fromString )
import Data.Monoid (mempty, (<>))

empty :: Doc ann 
empty = mempty

-- class Pretty a where pretty :: a -> Doc

x $$ y = D.vcat [x,y]
x <+> y = x D.<+> align y
vcat = align . D.vcat
hcat = align . D.hcat
vsep = align . D.vsep
hsep = align . D.hsep
fsep = align . D.fillSep
sep = align . D.sep

render = T.renderLazy . renderPretty

renderPretty = D.layoutPretty D.defaultLayoutOptions
renderCompact = D.layoutCompact
renderWide = D.layoutSmart $ D.LayoutOptions { D.layoutPageWidth = D.Unbounded }

displayIO = T.renderIO


text :: String -> D.Doc ann
text = fromString

instance ( Pretty a, Pretty b, Pretty c, Pretty d ) => Pretty (a,b,c,d) where
    pretty (x,y,z,u) = parens $ fsep $ punctuate comma [ pretty x, pretty y, pretty z, pretty u ]

-- | WARNING: there is  instance Pretty a => Pretty (Maybe a) in the back-end
-- but its spec is "Ignore Nothings, print Just contents"

instance ( Pretty a, Pretty b ) => Pretty (Either a b) where
    pretty (Left x) = text "Left" <+> parens (pretty x)
    pretty (Right x) = text "Right" <+> parens (pretty x)
