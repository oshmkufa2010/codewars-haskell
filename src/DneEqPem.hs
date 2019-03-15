{-# LANGUAGE RankNTypes #-}

module DneEqPem where

import Prelude hiding (undefined, error)
import Data.Void

-- absurd :: forall a. Void -> a
-- 

type AxiomPEM = forall a. forall b. (a -> b) -> ((a -> Void) -> b) -> b
type AxiomDNE = forall a. ((a -> Void) -> Void) -> a

from :: AxiomDNE -> AxiomPEM
from dne = \ab avb -> dne (\bv -> bv $ avb (bv . ab))

to :: AxiomPEM -> AxiomDNE
to pem = \avv -> pem id (\av -> absurd $ avv av)
