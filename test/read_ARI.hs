import TPDB.Data
import TPDB.Pretty
import TPDB.Input.File
import TPDB.Plain.Write

import Control.Monad ( forM, void )

main = void $ do
    p <- get_trs "test/01.ari"
    print $ pretty p


