import TPDB.Data
import TPDB.Pretty
import TPDB.XTC
import TPDB.Plain.Write

import Control.Monad ( forM, void )

main = void $ do
    p <- readProblemF "test/AC28.xml"
    print $ pretty p
    print $ full_signature p

