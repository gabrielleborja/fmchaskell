module Nat where
import Prelude hiding (sum, mult, exp, quot, min, gcd, lcm, div, max, pred, rem, minus, fact, fib, double, leq)


data Nat = O | S Nat
    deriving ( Prelude.Eq , Prelude.Show )

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
min O _ = O
min _ O = O
min(S n) (S m) = S (min n m)

max :: Nat -> Nat -> Nat
max O n = n 
max n O = n 
max (S n) (S m) = S(min n m)

monus :: Nat -> Nat -> Nat
monus n O = n 
monus O _ = O
monus (S n) (S m) = pred (pred (monus n m))





leq :: Nat -> Nat -> Prelude.Bool
leq O _ = Prelude.True
leq _ O = Prelude.False
leq (S n) (S m) = leq n m 

ev :: Nat -> Prelude.Bool
ev O = Prelude.True 
ev (S O) = Prelude.False
ev (S (S O)) = Prelude.True
ev (S (S n)) = ev n  

od :: Nat -> Prelude.Bool
od O = Prelude.False
od (S O) = Prelude.True
od (S (S O)) = Prelude.False
od (S (S n)) = od n

isZero :: Nat -> Prelude.Bool
isZero O = Prelude.True
isZero _ = Prelude.False
