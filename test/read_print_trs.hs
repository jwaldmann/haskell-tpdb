import TPDB.Plain.Write
import TPDB.Plain.Read
import TPDB.Pretty

import Data.Text.Lazy.IO as T
import Control.Monad ( forM, void )
import System.IO (stdout)

main = void $ do
    s <- T.readFile "test/33.trs"
    case trs s of
        Right t -> displayIO stdout $ renderWide $ pretty t
        Left err -> error err
