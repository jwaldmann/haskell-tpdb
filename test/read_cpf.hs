import qualified TPDB.CPF.Proof.Type as CPF
import qualified TPDB.CPF.Proof.Read as CPF
import TPDB.Pretty
import TPDB.Plain.Write

main = do
    p <- CPF.readFile "test/AC28.cpf"
    print $ pretty $ CPF.input p

