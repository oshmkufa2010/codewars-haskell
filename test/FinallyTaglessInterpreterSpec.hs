{-# LANGUAGE RankNTypes #-}

module FinallyTaglessInterpreterSpec where

import Test.Hspec hiding (before)
import FinallyTaglessInterpreter
import Prelude hiding (and, or)


spec :: Spec
spec = do
  describe "basic programs" $ do
    it "(\\ x . x) 3 is 3" 
      $ interpret (apply (lambda here) (int 3)) `shouldBe` 3
    it "if True then 3 else 4 is 3"
      $ shouldBe 
          (interpret (ifte (bool True) (int 3) (int 4)))
          3      
  describe "factorial" $ do
    it "factorial 10 is 3628800" 
    $ let eq0 :: Term (Int -> Bool)
          eq0 = apply ieq (int 0)
          ieq :: Term (Int -> Int -> Bool)
          ieq = 
            lambda $
              lambda $
                and (gte here (before here))
                    (gte (before here) here)
          fact_ :: Term ((Int -> Int) -> (Int -> Int))
          fact_ = 
            lambda $
              lambda $ 
                ifte (apply eq0 here) 
                     (int 1)
                     (mult here 
                           (apply (before here) (down here)))
          fact :: Term (Int -> Int)
          fact = loop fact_
          
      in interpret (apply fact (int 10)) 
         `shouldBe` 3628800