{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}

module AdditionAssocSpec (spec) where

import AdditionAssoc
-- import AdditionAssoc.Definitions

import Test.Hspec

-- | Verify that the functions' signature is correct:
solution :: Natural a -> Natural b -> Natural c -> Equal (a :+: (b :+: c)) ((a :+: b) :+: c)
solution = plusAssoc

spec :: Spec
spec = do
  describe "Proof checking" $ do
    it "Doesn't use any unsafe modules" $
      -- hidden [Module "Unsafe.Coerce"]
      True

