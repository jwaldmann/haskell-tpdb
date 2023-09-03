module TPDB.Data.Identifier where

import qualified Data.Text as T
import Data.Typeable
import Data.Hashable

data Identifier =
     Identifier { _identifier_hash :: !Int
                , name :: !T.Text
                , arity :: Int
                }
    deriving ( Eq, Ord, Typeable )

instance Hashable Identifier where
    hashWithSalt _ = _identifier_hash

instance Show Identifier where show = T.unpack . name

mk :: Int -> T.Text -> Identifier
mk a n = Identifier { _identifier_hash = hash (a,n)
                    , arity = a, name = n }
