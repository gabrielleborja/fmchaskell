module Nat where
import Prelude hiding (sum, mult, exp, quot, min, gcd, lcm, div, max, pred, rem, minus, fact, fib, double, True, Bool, False, if_then_else, leq)
import Bool

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

--Bool:

if_then_else :: Bool -> Nat -> Nat -> Nat
if_then_else True n _ = n
if_then_else False _ n = n

leq :: Nat -> Nat -> Bool
leq O _ = True
leq _ O = False
leq (S n) (S m) = leq n m 

ev :: Nat -> Bool
ev O = True 
ev (S O) = False
ev (S (S O)) = True
ev (S (S n)) = ev n  

od :: Nat -> Bool
od O = False
od (S O) = True
od (S (S O)) = False
od (S (S n)) = od n

isZero :: Nat -> Bool
isZero O = True
isZero _ = False
