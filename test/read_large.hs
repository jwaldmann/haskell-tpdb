import TPDB.Data
import TPDB.Pretty
import TPDB.XTC
import TPDB.Plain.Write

import Control.Monad ( forM, void )

main = void $ do
    p <- readProblemF "test/MNZ_10_labelled.xml"
    print $ pretty p


