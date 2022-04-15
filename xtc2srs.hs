import qualified TPDB.Data as D
import qualified TPDB.Input as I
import qualified TPDB.XTC as X
import qualified TPDB.Pretty as P
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as L
import System.IO (stdout)

main = do
  [f] <- getArgs
  s <- I.get_srs f
  P.displayIO stdout $ P.renderWide $ P.pretty s

