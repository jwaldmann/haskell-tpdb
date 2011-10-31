
import TPDB.Data

import TPDB.XTC
import TPDB.Plain.Write

import Control.Monad ( forM, void )

main = void $ do
    ps <- readProblems "test/3.15.xml"
    print $ length ps
    forM ps ( print . pretty )
