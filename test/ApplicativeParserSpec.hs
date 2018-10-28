module ApplicativeParserSpec where
    import Control.Monad
    import Data.Char
    import Data.Maybe

    import Test.Hspec

    import Test.QuickCheck

    import ApplicativeParser

    data Foo = Foo deriving Eq

    spec :: Spec
    spec = do
        describe "basics" $ do
            it "charP exact single parse" $ property $ \c -> unP (charP c) [c] == [([], c)]
            it "charP with rest" $ property $ \c i -> let rem = replicate i ' ' in unP (charP c) (c : rem) == [(rem, c)]
            it "charP fail" $ property $ \c d -> (c /= d) ==> (unP (charP c) [d, c] == [])

            it "charP empty input" $ unP (charP 'x') "" `shouldBe` []

        describe "pmap, inject and application" $ do
            it "pmap into list" $ property $ \c -> unP (pmap (: []) $ charP c) [c] == [("", [c])]

            it "pmap example with inject" $ unP (inject 42) "abc" == unP (pmap (+ 22) $ inject 20) "abc"

            it "<#" $ property $ forAll abcGen $ \c -> unP (Foo <# charP 'a') [c] == if c == 'a' then [("", Foo)] else []

        describe "other combinators" $ do

            it "stringP should parse string with same input" $ property $ \s -> runParserUnique (stringP s) s == Just s
            it "stringP shouldn't parse string with different input" $ property $
                \s1 s2 -> (s1 /= s2) ==> ((runParserUnique (stringP s1) s2) == Nothing)

        describe "combinators with choice" $ do
            it "emptyP should never parse anything" $ property $ \s -> runParserUnique (emptyP :: Parser ()) s == Nothing

            it "<<>> should include both results" $ property $ forAll (liftM (:[]) abcGen) $ \s -> forAll (twoOf abcGen) $ \(c1, c2) ->
                unP (charP c1 <<>> charP c2) s == unP (charP c1) s ++ unP (charP c2) s

        describe "running parsers" $ do
            let ambigP   = unambigP <<>> unambigP
                unambigP = charP 'a'

            it "runParserUnique should not allow multiple results" $ runParserUnique ambigP "a" `shouldBe` Nothing
            it "runParserUnique should work with a single result" $ runParserUnique unambigP "a" `shouldBe` Just 'a'

            it "runParser should allow multiple results" $ runParser ambigP "a" `shouldBe` ['a', 'a']
            it "runParser should work with a single result" $ runParser unambigP "a" `shouldBe` ['a']

        describe "expressions" $ do
            let interpretExpr = fmap evalExpr . parseExpr
            it "--1" $
                interpretExpr "--1" `shouldBe` Just 1
            it "(z + -z)" $
                interpretExpr "(z + -z)" `shouldBe` Just 0
            it "(2 + -1)" $
                interpretExpr "(2 + -1)" `shouldBe` Just 1
            it "((-(4 * 2) * z) + (2 + 3))" $
                interpretExpr "((-(4 * 2) * z) + (2 + 3))" `shouldBe` Just 5

            it "invalid binary op" $
                interpretExpr "(z / 1)" `shouldBe` Nothing
            it "needs parenthesis" $
                interpretExpr "z + 1" `shouldBe` Nothing
            it "only one whitespace as sep" $
                interpretExpr "(1 +  1)" `shouldBe` Nothing
            it "typo: zz" $
                interpretExpr "zz" `shouldBe` Nothing

        -- describe "do not cheat" $ do
        --     it "do not use Control.Applicative" $ hidden [Module "Control.Applicative"]
        --     it "do not use Data.Functor" $ hidden [Module "Data.Functor"]
        --     it "do not use fmap" $ hidden $ map (FromModule "Prelude") ["fmap"]
        where
        twoOf g     = liftM2 (,) g g

        numStrGen   = choose (1, 16) >>= \n -> vectorOf n $ elements ['0' .. '9']
        lowerStrGen = listOf $ elements ['a' .. 'z']
        abcGen      = elements "abcdecfgh"