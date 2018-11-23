module FunctionalStreamsSpec where

    import FunctionalStreams
    
    import Control.Applicative
    import Test.Hspec
    import Test.QuickCheck

    -- import Stream.Internal

    spec :: Spec
    spec = do
        describe "basics" $ do
            it "headS on Int" $ headS (42 :> undefined) `shouldBe` 42
            it "tail" $ afterHeadS ('a' :> 'b' :> undefined) `shouldBe` 'b'
        
        describe "peeking into streams" $ do
            it "takeS should work for negative indices" $ take 2 (takeS (-1) $ 1 :> 2 :> undefined) `shouldBe` []
    
        describe "stream constructors" $ do
            it "repeatS" $ headS (repeatS 42) `shouldBe` 42
            it "iterateS" $ afterHeadS (iterateS (+ 1) 0) `shouldBe` 1
            it "cycleS" $ afterHeadS (cycleS [1 .. 9]) `shouldBe` 2
            it "fromS" $ afterHeadS (fromS 42) `shouldBe` 43
            it "fromStepS" $ afterHeadS (fromStepS 42 2) `shouldBe` 44
    
        describe "general purpose functions" $ do
            it "foldrS" $ all (== 42) (take 10 $ foldrS (:) (repeatS 42)) `shouldBe` True
            it "filterS" $ takeS 4 (filterS even $ fromS 0) `shouldBe` [0, 2, 4, 6]
            it "takeS" $ length (takeS 5 $ repeatS 42) `shouldBe` 5
            it "dropS" $ headS (dropS 10 $ fromS 0) `shouldBe` 10
            it "splitAtS" $ case splitAtS 1 (fromS 0) of { (ls, rs) -> (ls, headS rs) `shouldBe` ([0], 1) }
            it "zipWithS" $ headS (zipWithS (+) (repeatS 20) (repeatS 22)) `shouldBe` 42
    
        describe "class instances" $ do
            it "fmap" $ headS (fmap (+ 1) $ repeatS 1) `shouldBe` 2
            it "pure" $ takeS 2 (pure 42) `shouldBe` [42, 42]
            it "(<*>)" $ headS (pure (* 2) <*> pure 21) `shouldBe` 42
    
        describe "sequences" $ do
            it "fibonacci sequence" $ takeS 4 fibS `shouldBe` [0, 1, 1, 2]
            it "prime sequence" $ takeS 10 primeS `shouldBe` [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
      where
        afterHeadS = headS . tailS