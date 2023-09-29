module Nat where
import Prelude hiding (sum, mult, exp, quot, min, gcd, lcm, div, max, pred, rem, minus, fact)

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

pred :: Nat -> Nat 
pred O = O
pred (S n) = n 

fact :: Nat -> Nat
fact O = S O
fact (S n) = mult (S n) (fact n)

double :: Nat -> Nat 
double O = O
double (S n) = S (S (double n)) 

fib :: Nat -> Nat 
fib O = O
fib (S O) = S O
fib (S (S n)) = sum (fib(S n)) (fib n)

min :: Nat -> Nat -> Nat
min O n = O
min n O = O
min(S n) (S m) = S (min n m)
