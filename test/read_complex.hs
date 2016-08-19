import TPDB.Data
import TPDB.Pretty
import TPDB.XTC
import TPDB.Plain.Write

import Control.Monad ( forM, void )

main = void $ do
    [ p ] <- readProblems "test/3.39.xml"
    print $ pretty p


