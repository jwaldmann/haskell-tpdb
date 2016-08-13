import qualified TPDB.Data as D
import qualified TPDB.Input as I
import qualified TPDB.XTC as X
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as L

main = do
  [f] <- getArgs
  s <- I.get_trs f
  L.putStrLn $ X.renderLBS X.def $ X.document
    $ D.Problem { D.type_ = D.Termination, D.trs = s
                , D.strategy = Just D.Full
                }
