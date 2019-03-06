{-# LANGUAGE TypeOperators, TypeFamilies, GADTs #-}

module AdditionAssoc where

-- import Kata.AdditionAssoc.Definitions

-- Preloaded code, might be helpful for
-- doing this Kata locally

-- | The natural numbers, encoded in types.
data Z
data S n

-- | Predicate describing natural numbers.
-- | This allows us to reason with `Nat`s.
data Natural :: * -> * where
  NumZ :: Natural Z
  NumS :: Natural n -> Natural (S n)

-- | Predicate describing equality of natural numbers.
data Equal :: * -> * -> * where
  EqlZ :: Equal Z Z
  EqlS :: Equal n m -> Equal (S n) (S m)

-- | Peano definition of addition.
type family (:+:) (n :: *) (m :: *) :: *
type instance Z :+: m = m
type instance S n :+: m = S (n :+: m)

-- Helpers

sPlus :: Natural a -> Natural b -> Natural (a :+: b)
sPlus NumZ n = n 
sPlus (NumS m) n = NumS (sPlus m n)

-- | For any n, n = n.
reflexive :: Natural n -> Equal n n
reflexive NumZ = EqlZ
reflexive (NumS n) = EqlS (reflexive n)

-- | if a = b, then b = a.
symmetric :: Equal a b -> Equal b a
symmetric EqlZ = EqlZ
symmetric (EqlS aeqb) = EqlS (symmetric aeqb)

-- This is the proof that the kata requires.
-- | a + (b + c) = (a + b) + c
plusAssoc :: Natural a -> Natural b -> Natural c -> Equal (a :+: (b :+: c)) ((a :+: b) :+: c)
plusAssoc NumZ NumZ NumZ = EqlZ
plusAssoc NumZ NumZ (NumS nc) = EqlS (plusAssoc NumZ NumZ nc)
plusAssoc NumZ (NumS nb) nc = EqlS (plusAssoc NumZ nb nc)
plusAssoc (NumS na) nb nc = EqlS (plusAssoc na nb nc)
