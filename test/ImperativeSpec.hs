module ImperativeSpec where
  import Imperative (def, var, lit, while, (+=), (-=), (*=))
  import Test.Hspec
  
  -- foldr (*) 1
  factorial :: Integer -> Integer
  factorial n = def $ do
    result <- var 1
    i      <- var n
    while i (>0) $ do
      result *= i
      i      -= lit 1
    return result
   
  -- ((max 0 . subtract 1) .) . subtract
  howManyBetween :: Integer -> Integer -> Integer
  howManyBetween c n = def $ do
    result <- var 0
    i      <- var (c + 1)
    while i (<n) $ do
      result += lit 1
      i      += lit 1
    return result
  
  spec :: Spec
  spec = do
    describe "factorial" $ do
      it "should return the same as the functional one" $ do
        factorial 10 `shouldBe` foldr (*) 1 [1..10]
    describe "howManyBetween" $ do
      it "should return the same as the functional one" $ do
        howManyBetween 1 10 `shouldBe` (max 0 $ 10 - 1 - 1 :: Integer)

