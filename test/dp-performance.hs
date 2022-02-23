import TPDB.Data (rules)
import TPDB.Plain.Write
import TPDB.Plain.Read
import TPDB.Pretty

import qualified TPDB.DP.Transform as DT
import qualified TPDB.DP.Graph as DG
import qualified TPDB.DP.Usable as DU

import Data.Either
import Data.Text.Lazy.IO as T
import Control.Monad ( forM, void )
import System.IO (stdout)
import Text.Printf

main = void $ do
    s <- T.readFile "test/labelled.trs"
    case trs s of
      Left err -> error err
      Right r -> do
        printf "R has %d rules\n" (length $ rules r)
        let d = DT.dp r
        printf "DP(R) has %d rules\n" (length $ rules d)
        let c = rights $ DG.components d
        printf "EDG(R) has %d cyclic components with sizes %s\n"
          (length c) (show $ map (length . rules) c)
        let u = map DU.restrict c
        printf "usable sizes %s\n" (show $ map (length . rules) u)
