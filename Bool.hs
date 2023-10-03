module Bool where
import Prelude hiding (if_then_else, leq, ev, od, isMul, divides, isZero)


data Bool = False | True 
    deriving ( Eq , Show)

