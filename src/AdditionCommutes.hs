{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module AdditionCommutes
where

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

-- These are some lemmas that may be helpful.
-- They will *not* be tested, so rename them
-- if you so desire. Good luck!

-- | For any n, n = n.
refl :: Natural n -> Equal n n
refl NumZ =  EqlZ
refl (NumS n) = EqlS (refl n) 

-- | if a = b, then b = a.
symm :: Equal a b -> Equal b a
symm EqlZ = EqlZ
symm (EqlS eqab) = EqlS (symm eqab)

-- | if a = b and b = c, then a = c.
trans :: Equal a b -> Equal b c -> Equal a c
trans EqlZ EqlZ = EqlZ 
trans (EqlS aeqb) (EqlS beqc) = EqlS (trans aeqb beqc)

-- | a + 0 = a
rightUnit :: Natural a -> Equal (a :+: Z) a
rightUnit NumZ = EqlZ
rightUnit (NumS na) = EqlS (rightUnit na)

-- | (a + b) ++ = a + (b ++)
lemma :: Natural a -> Natural b -> Equal (S (a :+: b)) (a :+: S b)
lemma NumZ nb = EqlS (refl nb)
lemma (NumS na) nb = EqlS (lemma na nb)

-- This is the proof that the kata requires.
-- | a + b = b + a
plusCommutes :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a)
plusCommutes NumZ NumZ = EqlZ
plusCommutes (NumS na) NumZ = EqlS $ plusCommutes na NumZ
plusCommutes na (NumS nb) = symm (lemma na nb) `trans` EqlS (plusCommutes na nb)
