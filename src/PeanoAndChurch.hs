{-# LANGUAGE 
  FlexibleInstances, 
  UndecidableInstances, 
  InstanceSigs,
  ScopedTypeVariables,
  RankNTypes #-}

module PeanoAndChurch where

import Data.List

type ISO a b = (a -> b, b -> a)
-- See https://www.codewars.com/kata/isomorphism

symm :: ISO a b -> ISO b a
symm (ab, ba) = (ba, ab)

substL :: ISO a b -> (a -> b)
substL = fst

substR :: ISO a b -> (b -> a)
substR = snd

liftISO2 :: ISO a b -> ISO (a -> a -> a) (b -> b -> b)
liftISO2 (ab, ba) = (\aaa b b' -> ab (aaa (ba b) (ba b')), \bbb a a' -> ba (bbb (ab a) (ab a')))

-- A Natural Number is either Zero,
-- or a Successor (1 +) of Natural Number.
-- We have (+)/(*) on Natural Number, or (-) it.
-- Since Natrual Number do not have negative, forall x, 0 - x = 0.
-- We also have pow on Natrual Number
-- Since Haskell is lazy, we also have infinity

class Nat n where
  zero :: n
  successor :: n -> n
  nat :: a -> (n -> a) -> n -> a -- Pattern Matching
  iter :: a -> (a -> a) -> n -> a -- Induction
  plus, minus, mult, pow :: n -> n -> n
  inf :: n
  inf = successor inf
  divide :: n -> n -> n
  l `divide` r | l == 0 && r == 0 = undefined
  l `divide` r | l < r = 0
  l `divide` r | otherwise = successor $ (l `minus` r) `divide` r
  -- notice (l `divide` 0) when l is not 0 will return inf
  isoP :: ISO n Peano -- See below for the definition of Peano
  isoP = (iter zero successor, iter zero successor)
  toP :: n -> Peano
  toP = substL isoP

instance {-# OVERLAPPABLE #-} Nat n => Show n where
  show = show . toP

instance {-# OVERLAPPABLE #-} Nat n => Eq n where
  l == r = toP l == toP r

instance {-# OVERLAPPABLE #-} Nat n => Ord n where
  l `compare` r = toP l `compare` toP r

instance {-# OVERLAPPABLE #-} Nat n => Num n where
  abs = id
  signum = nat zero (const 1)
  fromInteger 0 = zero
  fromInteger n | n > 0 = successor $ fromInteger (n - 1)
  (+) = plus
  (*) = mult
  (-) = minus

-- We can encode Natrual Number directly as Algebraic Data Type(ADT).
data Peano = O | S Peano deriving (Show, Eq, Ord)

-- Remember, 0 - x = 0 for all x.
instance Nat Peano where
  zero = O
  successor = S

  nat a na n = case n of
    O -> a
    S n' -> na n'

  iter a aa n = case n of
    O -> a
    S n' -> aa (iter a aa n')
  
  plus n1 n2 = case n1 of
    O -> n2
    S n1' -> S (plus n1' n2)
  
  minus O _ = O
  minus n O = n
  minus (S n1) (S n2) = minus n1 n2
  
  mult O _ = O
  mult n1 n2 = case n1 of
    O -> O
    S n1' -> plus (mult n1' n2) n2
  
  pow n1 n2 = case n2 of
    O -> S O
    S n2' -> mult (pow n1 n2') n1
  
-- Peano is very similar to a basic data type in Haskell - List!
-- O is like [], and S is like :: (except it lack the head part)
-- When we want to store no information, we can use (), a empty tuple
-- This is different from storing nothing (called Void in Haskell),
-- as we can create a value of () by using (), 
-- but we cannot create a value of Void.

-- Notice how you can implement everything once you have isoP,
-- By converting to Peano and using Nat Peano?
-- Dont do that. You wont learn anything.
-- Try to use operation specific to list.
instance Nat [()] where
  zero = []
  successor n = () : n

  nat a na n = case n of
    [] -> a
    () : n' -> na n'
  
  iter a aa n = case n of
    [] -> a
    () : n' -> aa (iter a aa n')
  
  plus n1 n2 = n1 ++ n2

  minus [] _ = []
  minus n [] = n
  minus (() : n1) (() : n2) = minus n1 n2
  
  mult n1 n2 = case n1 of
    [] -> []
    () : n1' -> plus (mult n1' n2) n2
  
  pow n1 n2 = case n2 of
    [] -> [()] 
    () : n2' -> mult (pow n1 n2') n1

-- Instead of defining Nat from zero, sucessor (and get Peano),
-- We can define it from Pattern Matching
newtype Scott = Scott { runScott :: forall a. a -> (Scott -> a) -> a }
instance Nat Scott where
  zero = Scott const
  successor s = Scott $ \_ sa -> sa s
  nat a sa (Scott f) = f a sa
  iter a aa (Scott f) = f a $ \s -> iter (aa a) aa s
  -- Other operation on Scott numeral is sort of boring,
  -- So we implement it using operation on Peano.
  -- You shouldnt do this - I had handled all the boring case for you.
  plus = substR (liftISO2 isoP) plus
  minus = substR (liftISO2 isoP) minus
  mult = substR (liftISO2 isoP) mult
  pow = substR (liftISO2 isoP) pow

-- Or from induction!
newtype Church = Church { runChurch :: forall a. (a -> a) -> a -> a }
instance Nat Church where
  -- Try to implement the calculation (except minus) in the primitive way.
  -- Implement them by constructing Church explicitly.
  -- So plus should not use successor,
  -- mult should not use plus,
  -- exp should not use mult.
  zero = Church $ flip const
  successor (Church f) = Church $ \aa a -> aa (f aa a)
  nat a ca c = nat a (ca . substR isoP) $ toP c
  iter a aa (Church f) = f aa a
  plus (Church f1) (Church f2) = Church $ \aa a -> f1 aa (f2 aa a)
  minus = substR (liftISO2 isoP) minus
  mult (Church f1) (Church f2) = Church $ \aa -> f2 (f1 aa)
  pow (Church f1) (Church f2) = Church $ f2 f1
