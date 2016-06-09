import TPDB.Plain.Write
import TPDB.Plain.Read
import TPDB.Pretty

import Data.ByteString.Lazy as B
import Control.Monad ( forM, void )

main = void $ do
    s <- B.readFile "test/33.trs"
    case trs s of
        Right t -> print $ pretty t
        Left err -> error err
