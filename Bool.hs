{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Bool where
import Prelude hiding (Bool, True, False, if_then_else2, if_then_else)
import Nat

data Bool = False | True 
    deriving ( Eq , Show)

--função desenvolvida em grupo com outros alunos
if_then_else2 :: Bool -> Bool -> Bool -> Bool
if_then_else2 True n _ = n
if_then_else2 False _ m = m

if_then_else :: Bool -> Nat -> Nat -> Nat
if_then_else True n _ = n
if_then_else False _ n = n