{-# language PatternSignatures #-}

module Rainbow.Proof.Statistics where

import Rainbow.Proof.Type
import Rainbow.Proof.Xml

import Autolib.ToDoc

import Data.Map ( Map )
import qualified Data.Map as M

import Data.Set ( Set )
import qualified Data.Set as S
import Text.XML.HaXml.Haskell2Xml

import System.Environment
import Data.Time
import System.IO

import qualified Control.Exception

main :: IO ()
main = do
    args <- getArgs
    infos <- mapM collect args
    print $ present $ concat infos

present infos = vcat $ do
    let fm = M.fromListWith S.union infos
    ( ( dom, dim ), entries ) <- M.toList fm
    return $ vcat
	   [ toDoc dom <+> toDoc dim
	   , nest 4 $ describe entries
	   ]
       
describe entries = 
    let xs = S.toList entries
	m = xs !! ( length xs `div` 2 )
    in  vcat [ text  "total :" <+> toDoc ( length xs )
	     , text  "max   :" <+> toDoc ( last xs )
	     , if length xs > 1
	       then text "second:" <+> toDoc ( xs !! (length xs - 2))
	       else empty
	     , text  "median:" <+> toDoc ( m )
	     ]


-- | find data for proof from this file 
collect :: FilePath 
	-> IO [ ((Domain,Integer), Set (NominalDiffTime, FilePath)) ]
collect file = do  
       p :: Proof <- fReadXml file 
       let res = walk file p
       return $ length res `seq` res
  `Control.Exception.catch` \ ( err :: Control.Exception.SomeException ) -> do
    hPutStrLn stderr $ unlines
        [ "error: " ++ show err
        , "file : " ++ file
        ]
    return []

walk file p = case reason p of
    Trivial -> []
    MannaNess o p -> info file o : walk file p
    MarkSymb p -> walk file p
    DP p -> walk file p
    As_TRS p -> walk file p
    Reverse p -> walk file p

info file ( Red_Ord_Matrix_Int m ) = case m of 
    Matrix_Int { mi_domain = d , mi_dim = i, mi_duration = u } ->
        (( d, i ), S.singleton ( u, file ))

