import TPDB.Data
import TPDB.Pretty
import TPDB.XTC
import TPDB.Plain.Write

import Control.Monad ( forM, void )

main = void $ do
    p <- readProblemF "test/3.15.xml"
    print $ pretty p
    print $ pretty $ attributes p
