{-# language UndecidableInstances, OverlappingInstances, IncoherentInstances, FlexibleInstances, ScopedTypeVariables, OverloadedStrings #-}

module TPDB.Xml

( XmlContent (..), mkel, rmkel
, content, (&|)
  , escape, nospaceString
)
  
where

import Data.Typeable

import Control.Monad
import Control.Applicative

import Text.XML
import Text.XML.Cursor

import Data.String
import qualified Data.Text as T

class XmlContent a where
  toContents :: a -> [ Node ]
  parseContents :: Cursor -> [a]

instance XmlContent Int where
  toContents = return . nospaceString . fromString . show
instance XmlContent Integer where
  toContents = return . nospaceString . fromString . show
instance XmlContent Bool where
  toContents False = return $ nospaceString "false"
  toContents True = return $ nospaceString "true"

mkel name cs = NodeElement $ Element name mempty cs 
rmkel name cs = return $ mkel name cs
       
nospaceString :: T.Text -> Node
nospaceString = NodeContent 

{-


instance Typeable t => HTypeable t where 
    toHType x = let cs = show ( typeOf x ) in Prim cs cs
-}

escape [] = []
escape ( c : cs ) = case c of
    '<' -> "&lt;" ++ escape cs
    '>' -> "&gt;" ++ escape cs
    _   -> c :       escape cs

{-
type Contents = [ Content Posn ]

data CParser a = CParser { unCParser :: Contents -> Maybe ( a, Contents ) }

instance Functor CParser where
    fmap f (CParser p) = CParser $ \ cs ->
        do ( x, cs' ) <- p cs ; return ( f x, cs' )

instance Applicative CParser where
    pure = return ; (<*>) = ap
         
instance Monad CParser where
    return x = CParser $ \ cs -> return ( x, cs )
    CParser p >>= f = CParser $ \ cs0 -> 
        do ( x, cs1 ) <- p cs0 ; unCParser ( must_succeed $ f x ) cs1


must_succeed :: CParser a -> CParser a
must_succeed (CParser p ) = CParser $ \ cs -> 
    case p cs of
        Nothing -> error $ "must succeed:" ++ errmsg cs
        ok -> ok

class Typeable a => XRead a where xread :: CParser a


instance ( Typeable a, XmlContent a ) => XRead a where
    xread = CParser $ \ cs -> case runParser parseContents cs of
          ( Right x, rest ) -> Just ( x, rest )  
          ( Left err, rest ) -> Nothing

wrap :: forall a . Typeable a => CParser a -> Parser ( Content Posn ) a
wrap ( CParser p ) = P $ \ cs -> case p cs of
     Nothing -> Failure cs $ unlines
             $ "want expression of type " 
             :  show ( typeOf ( undefined :: a )) 
             :  errmsg cs
             : []
     Just ( x, cs' ) -> Committed ( Success cs' x )

errmsg cs = unlines $ case cs of 
                  ( c  : etc ) -> 
                     [ show $ P.content c
                    
                     ]
                  _ -> [ show $ length cs ]

orelse :: CParser a -> CParser a  -> CParser a
orelse ( CParser p ) ( CParser q ) = CParser $ \ cs -> 
    case p cs of Nothing -> q cs ; ok -> ok

many :: CParser a -> CParser [a]
many p = ( do x <- p ; xs <- TPDB.Xml.many p ; return $ x : xs ) `orelse` return []

element tag p = element0 (N tag) $ must_succeed p

element0 tag p = CParser $ \ cs -> case strip cs of
     ( CElem ( Elem name atts con ) _ : etc ) | name == tag -> 
         case unCParser p con of
             Nothing -> Nothing
             Just ( x, _ ) -> Just ( x, etc )
     _ -> Nothing

strip [] = []
strip input @ ( CElem ( Elem {} ) _ : _ ) = input
strip (c : cs) = strip cs

xfromstring :: Read a => CParser a
xfromstring = CParser $ \ cs -> case cs of
    ( CString _ s _ : etc ) -> Just ( read s, etc )
    _ -> Nothing

complain :: String -> CParser a
complain tag = CParser $ \ cs -> error $ "ERROR: in branch for " ++ tag ++ errmsg cs

info :: Contents -> String
info [] = "empty contents"
info ( c : cs ) = case c of
    CElem ( Elem name atts con ) _ -> "CElem, name: " ++ show name
    CString _ s _ -> "CString : " ++ s
    CRef _ _ -> "CRef"
    CMisc _ _ -> "CMisc"

-}

