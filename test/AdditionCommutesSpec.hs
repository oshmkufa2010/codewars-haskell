{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}

module AdditionCommutesSpec (spec) where

import AdditionCommutes

import Test.Hspec

-- | Verify that the functions' signature is correct:
solution :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a)
solution = plusCommutes

spec :: Spec
spec = do
  describe "Proof checking" $ do
    it "Doesn't use any unsafe modules" $
      -- hidden [Module "Unsafe.Coerce"]
      True
