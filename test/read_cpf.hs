import qualified TPDB.CPF.Proof.Type as CPF
import qualified TPDB.CPF.Proof.Read as CPF
import TPDB.Pretty
import TPDB.Plain.Write

main = do
    s <- readFile "test/236262878.cpf"
    ps <- CPF.readCP s
    case ps of
       [p] -> print $ pretty $ CPF.trsinput_trs $ CPF.input p
       _ -> print $ length ps

