module ListNat where
import Prelude hiding (Nil, length, sum, exp, product, elem, (++), append, reverse, allEven, True, Bool, False, anyEven, allOdd, anyOdd, allZero, anyZero, multNat, expNat)
import Nat
import Bool

data ListNat = Empty | Cons Nat ListNat
    deriving ( Eq, Show )

length :: ListNat -> Nat
length Empty = O
length (Cons _ l) = S (length l)

sumList :: ListNat -> Nat
sumList Empty = O
sumList (Cons n ns) = sum n (sumList ns)

product :: ListNat -> Nat
product Empty = (S O)
product (Cons n ns) = mult n (product ns)

elem :: Nat -> ListNat -> Bool
elem _ Empty = False
elem n (Cons x xs) = if (n == x) then True else (elem n xs)

--concatenação binária:
(++) :: ListNat -> ListNat -> ListNat
Empty ++ l = l
(Cons l ls) ++ xs = Cons l (ls ++ xs)

--append
append :: Nat -> ListNat -> ListNat
append n Empty = (Cons n Empty)
append n (Cons x xs) = Cons x (append n xs)

reverse :: ListNat -> ListNat
reverse Empty = Empty
reverse (Cons x xs) = append x (reverse xs)

allEven :: ListNat -> Bool
allEven Empty = True 
allEven (Cons x xs) = if_then_else2 (ev x) (allEven xs) False   

anyEven :: ListNat -> Bool
anyEven Empty = False
anyEven (Cons x xs) = if_then_else2 (ev x) True (anyEven xs)

allOdd :: ListNat -> Bool
allOdd Empty = True
allOdd (Cons x xs) = if_then_else2 (od x) (allOdd xs) False 

anyOdd :: ListNat -> Bool
anyOdd Empty = False
anyOdd (Cons x xs) =  if_then_else2 (od x) True (anyOdd xs)

allZero :: ListNat -> Bool
allZero Empty = True
allZero (Cons x xs) = if_then_else2 (isZero x) (allZero xs) False 

anyZero :: ListNat -> Bool 
anyZero Empty = False
anyZero (Cons x xs) = if_then_else2 (isZero x) True (anyZero xs)

addNat :: Nat -> ListNat -> ListNat
addNat n Empty = Empty
addNat n (Cons x xs) = Cons (sum n x) (addNat n xs) 

multNat :: Nat -> ListNat -> ListNat
multNat n Empty = Empty
multNat n (Cons x xs) = Cons (mult n x) (multNat n xs) 

expNat :: Nat -> ListNat -> ListNat
expNat n Empty = Empty
expNat n (Cons x xs) = Cons (exp n x) (expNat n xs) 
