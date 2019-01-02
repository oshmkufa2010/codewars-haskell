{-# LANGUAGE NoImplicitPrelude, GADTs , DataKinds, TypeFamilies, TypeOperators, RankNTypes, DeriveFunctor #-}

module Singletons where

import Prelude hiding (drop, take, head, tail, index, zipWith, replicate, map, (++))

data Vec a n where
  VNil :: Vec a Zero
  VCons :: a -> Vec a n -> Vec a (Succ n)

-- promoted to type level by data kinds
data Nat = Zero | Succ Nat

data SNat a where
  SZero :: SNat Zero
  SSucc :: SNat a -> SNat (Succ a)

type family (a :: Nat) :< (b :: Nat) :: Bool
type instance m :< Zero = False
type instance Zero :< Succ n = True
type instance (Succ m) :< (Succ n) = m :< n

type family (Add (a :: Nat) (b :: Nat)) :: Nat
type instance (Add Zero n) = n 
type instance (Add (Succ m) n) = Succ (Add m n)

type family (Min (a :: Nat) (b :: Nat)) :: Nat
type instance (Min Zero n) = Zero
type instance (Min m Zero) = Zero
type instance (Min (Succ m) (Succ n)) = Succ (Min m n)

type family (Sub (a :: Nat) (b :: Nat)) :: Nat
type instance (Sub Zero n) = Zero
type instance (Sub m Zero) = m
type instance (Sub (Succ m) (Succ n)) = Sub m n

map :: (a -> b) -> Vec a n -> Vec b n
map f VNil = VNil
map f (VCons x xs) = VCons (f x) (map f xs)

index :: ((a :< b) ~ True) => SNat a -> Vec s b -> s
index SZero (VCons x _) = x
index (SSucc n) (VCons _ xs) = index n xs

replicate :: s -> SNat a -> Vec s a
replicate v SZero = VNil
replicate v (SSucc n) = VCons v (replicate v n)

-- Both vectors must be of equal length
zipWith :: (a -> b -> c) -> Vec a n -> Vec b n -> Vec c n 
zipWith f VNil VNil = VNil
zipWith f (VCons x xs) (VCons y ys) = VCons (f x y) (zipWith f xs ys)

(++) :: Vec v m -> Vec v n -> Vec v (Add m n)
VNil ++ b = b
VCons x xs ++ b = VCons x (xs ++ b)

-- The semantics should match that of take for normal lists.
take :: SNat a -> Vec s b -> Vec s (Min a b)
take SZero _ = VNil
take _ VNil = VNil
take (SSucc n) (VCons x xs) = VCons x (take n xs)

-- The semantics should match that of drop for normal lists.
drop :: SNat a -> Vec s b -> Vec s (Sub b a)
drop SZero vec = vec
drop _ VNil = VNil
drop (SSucc n) (VCons x xs) = drop n xs

head :: ((Zero :< a) ~ True) => Vec s a -> s
head (VCons x xs) = x

tail :: Vec s (Add (Succ Zero) b) -> Vec s b
tail (VCons x xs) = xs
