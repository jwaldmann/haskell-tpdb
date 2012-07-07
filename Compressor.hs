import TPDB.Compress (compress)
import TPDB.DP ( dp, Marked (Auxiliary) )
import TPDB.Data
import TPDB.XTC.Read ( readProblems )
import TPDB.Plain.Write ( )
import TPDB.Pretty ( pretty )

import System.Environment
import System.IO
import System.Directory
import Control.Monad ( guard, forM )
import Data.List ( isSuffixOf, isPrefixOf )
import qualified Data.Set as S

-- | give file/dir name(s) on cmd line.
-- recursively collect all *.xml files,
-- and apply TRS compression.

main = do
    hSetBuffering stdout LineBuffering 
    args <- getArgs
    ss <- collect args
    forM ( map dp ss ) $ \ s -> do
        -- print $ pretty s 
        -- print $ pretty ( compress supply s)
        print $ judge s


judge sys = 
    let csys = compress supply sys
    in  ( cost_trs sys , cost_comp csys )

supply = do 
    i <- [ 0 .. ] 
    return $ Auxiliary $ mknullary $ "c" ++ show i

cost_comp ( s, sub ) = 
    cost_trs s + cost_sub sub
    
cost_sub sub = sum $ do
    ((f, af), Just (g, k, h, ca)) <- sub
    return ca

cost_trs s = sum $ do
    u <- rules s
    t <- [ lhs u, rhs u ]
    return $ cost_term t

cost_term t = sum $ do 
    (p,s) <- positions t 
    return $ multiplications s

multiplications t = case t of
    Node f xs -> sum $ do
        x @ Node {} <- xs
        return $ S.size $ vars x
    _ -> 0    

collect fs = do
    let special d = isPrefixOf "." d
    fss <- forM fs $ \ f -> do
        d <- doesDirectoryExist f
        if d then do
           fs <- getDirectoryContents f
           collect $ map ( ( f ++ "/" ) ++ )
                   $ filter (not . special) fs
        else if isSuffixOf ".xml" f
             then fmap (map trs) $ readProblems f
             else return []
    return $ concat fss

