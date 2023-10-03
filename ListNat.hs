module ListNat where
import Prelude hiding (Nil, length, sum, product, elem, (++), append, reverse, allEven, even, True, Bool, False)
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

