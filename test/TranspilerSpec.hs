module TranspilerSpec where

  import Transpiler (transpile)
  import Test.Hspec
  
  s =>> c = transpile s `shouldBe` Right c
  gg s = transpile s `shouldBe` Left "Hugh?"
  
  spec :: Spec
  spec = do
    describe "When there's no lambda" $ do
      it "Should work when expressions are very simple" $ do
        "call()" =>> "call()"
      it "Should not parse wtf" $ do
        gg "%^&*("
        gg "x9x92xb29xub29bx120()!("
      it "Should work when there're lots of spaces" $ do
        "invoke  (       a    ,   b   )" =>> "invoke(a,b)"
      it "Should work when there're multiple parameters" $ do
        "invoke(a, b)" =>> "invoke(a,b)"
  
    describe "When there're lambda expressions" $ do
      it "Should work for simple tests" $ do
        "call({})" =>> "call((){})"
      it "Should work for lambdas with single paramter" $ do
        "f({a->})" =>> "f((a){})"
      it "Should work when lambda has an expression inside" $ do
        "f({a->a})" =>> "f((a){a;})"
  
    describe "When lambda expressions aren't inside brackets" $ do
      it "Should work for empty lambdas" $ do
        "call(\n){}" =>> "call((){})"
      it "Should work when there're lots of spaces" $ do
        "invoke  (       a    ,   b   ) { } " =>> "invoke(a,b,(){})"
      it "Should work when there're parameters" $ do
        "f(x){a->}" =>> "f(x,(a){})"
      it "Should work when there's a statement inside lambda" $ do
        "f(a,b){a->a}" =>> "f(a,b,(a){a;})"
      it "Should work when there're no parameters but statements" $ do
        "run{a}" =>> "run((){a;})"
  
    describe "When invoking a lambda directly" $ do
      it "Should work when it's bare" $ do
        "{}()" =>> "(){}()"
      it "Should work when there're parameters" $ do
        "{a->a}(233)" =>> "(a){a;}(233)"
  --
  