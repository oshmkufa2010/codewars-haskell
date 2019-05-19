{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module YonedaLemma where
-- import YonedaLemmaPreloaded
import Data.Functor.Contravariant
import Data.Void
import Data.Proxy

-- coerce :: Count a -> Count b
-- instance (Factor f, Countable c) => Countable (f c) where count = factor

-- Hom(a, b) ≡ all arrows/morphisms from object `a` to object `b`
-- in given category.
-- Hom(a, -) covariant functor:
type Hom a = (->) a

-- natural transformation from functor f to functor g:
type Nat f g = forall x. f x -> g x

-- in order to witness isomorphism
-- we should provide `to` and `from` such, that
-- to . from ≡ id[f a]
-- from . to ≡ id[Nat (Hom a) f]
-- nat :: forall x. (a -> x) -> f x
to :: Functor f => Nat (Hom a) f -> f a
to nat = nat id

from :: Functor f => f a -> Nat (Hom a) f
from fa = \ax -> fmap ax fa

-- Hom(-, a) contravariant functor:
type CoHom a = Op a
{- NOTE:
Op a b = Op { getOp :: b -> a }

class Contravariant f where
  contramap :: (b -> a) -> f a -> f b
-}
-- nat :: forall x. (x -> a) -> f x
to' :: Contravariant f => Nat (CoHom a) f -> f a
to' nat = nat $ Op id

from' :: Contravariant f => f a -> Nat (CoHom a) f
from' fa = \xa -> contramap (getOp xa) fa

