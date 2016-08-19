import qualified TPDB.CPF.Proof.Type as CPF
import qualified TPDB.CPF.Proof.Read as CPF
import TPDB.Pretty
import TPDB.Plain.Write

main = do
    s <- readFile "test/AC28.cpf"
    [p] <- CPF.readCP_with_tracelevel 0 s
    print $ pretty $ CPF.input p

