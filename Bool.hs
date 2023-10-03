module Bool where
import Prelude hiding (Bool, True, False, if_then_else2)


data Bool = False | True 
    deriving ( Eq , Show)

if_then_else2 :: Bool -> Bool -> Bool -> Bool
if_then_else2 True n _ = n
if_then_else2 False _ m = m