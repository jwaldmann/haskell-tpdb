import Text.XML.HaXml

import qualified Text.XML.HaXml.Pretty as P
import qualified Text.XML.HaXml.ByteStringPP as BSP
import qualified Data.ByteString.Lazy.Char8 as BS
import Text.PrettyPrint.HughesPJ hiding ( int, double )

import qualified TPDB.Pretty as TP
import qualified TPDB.Xml.Pretty as TXP

import System.IO ( stdout )
import System.Environment ( getArgs )

mkel name cs = CElem ( Elem (N name) [] cs ) ()
rmkel name cs = return $ mkel name cs

list xs = foldr ( \ x y -> rmkel "cons" 
            [ mkel "head" x , mkel "tail" y ] )
        ( rmkel "nil" [] ) xs

int i = [ CElem (Elem (N "int") 
    [ (N "val", AttValue [ Left $ show i] ) ] []) () ]

double n = header
         $ list $ replicate n 
         $ list $ replicate n $ int 42

header l =
    let xd = XMLDecl "1.0" 
             ( Just $ EncodingDecl "UTF-8" ) Nothing 
        pro = Prolog ( Just xd ) [] Nothing []
        [ CElem e _ ] =  l
    in  Document pro emptyST e []

main = do
    -- print $ P.document
    -- putStrLn $ renderStyle (Style LeftMode undefined undefined ) $ P.document
    -- BS.putStrLn $ BSP.document
    TP.displayIO stdout $ TP.renderCompact $ TXP.document
            $  double 132