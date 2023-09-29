module Nat where
import Prelude hiding (sum, mult, exp, quot, min, gcd, lcm, div, max, pred, rem, minus)

data Nat = O | S Nat
    deriving ( Eq , Show )

sum :: Nat -> Nat -> Nat
sum n O = n
sum n (S m) = S (sum n m)

mult :: Nat -> Nat -> Nat
mult n O = O
mult n (S m) = sum n (mult n m)

exp :: Nat -> Nat -> Nat 
exp n O = S O
exp n (S O) = n 
exp n (S x) = mult n (exp n x)


