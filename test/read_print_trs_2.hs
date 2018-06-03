import TPDB.Plain.Write
import TPDB.Plain.Read
import TPDB.Pretty

import Data.Text.Lazy.IO as T
import Control.Monad ( forM, void )

main = void $ do
    s <- T.readFile "test/02.trs"
    case trs s of
        Right t -> print $ pretty t
        Left err -> error err
