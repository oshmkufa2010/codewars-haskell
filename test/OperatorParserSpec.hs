module OperatorParserSpec where
    import OperatorParser

    import Test.Hspec
    import Data.List
    import Data.Char

    arithOps :: [Associativity [ReadP String]]
    arithOps = 
        map (fmap (map (\s -> op s s) . words)) 
            [ R "&& ||", NoAssociativity "< > =", L "+ -", L "* /", R "^"]

    arithParser :: String -> String
    arithParser s = 
        case readP_to_S (parseOperators arithOps (munch1 isDigit) <* eof) s of
            [] -> ""
            xs -> brackets $ fst (last xs)

    brackets :: OpTree String String -> String
    brackets = foldTree (\o x y -> '(' : x ++ o ++ y ++ ")")

    parsesTo :: Eq a => (a, String) -> [(a, String)] -> Bool
    parsesTo _ [] = False 
    parsesTo x xs = last xs == x

    exampleTree :: OpTree String String
    exampleTree = 
        Op (Op (Term "1") "*" (Term "2")) 
            "+" 
            (Op (Term "3") "/" (Op (Term "5") "^" (Term "6")))

    spec :: Spec
    spec = do 
        describe "The op function" $ do
            it "parses an operator correctly" $
                readP_to_S (op ":<" ()) ":<xx" `shouldSatisfy` parsesTo ((), "xx")
            it "fails on incorrect input" $
                readP_to_S (op ":<" ()) ":xx" `shouldBe` []
            it "does not consume trailing whitespace" $
                readP_to_S (op ":<" ()) ":<  " `shouldSatisfy` parsesTo ((), "  ")
            it "fails if there is preceding whitespace" $
                readP_to_S (op ":<" ()) "   :<" `shouldBe` []
        
        describe "The foldTree function" $
            it "works as expected with the 'brackets' function" $
                brackets exampleTree `shouldBe` "((1*2)+(3/(5^6)))"

        describe "The parseOperators function" $ do
            it "parses a single term" $
                arithParser "12" `shouldBe` "12"
            it "parses expressions with no whitespace" $
                arithParser "1+2" `shouldBe` "(1+2)"
            it "fails if there is preceding whitespace" $
                arithParser "  1 + 1" `shouldBe` ""
            it "fails on incomplete expressions" $ do
                arithParser "* 4" `shouldBe` ""
            it "parses a simple expression separated by whitespace" $
                arithParser "1 \n+ 1" `shouldBe` "(1+1)"
            it "parses with correct precedence if lower precedence on the left" $
                arithParser "1 + 3 * 2" `shouldBe` "(1+(3*2))"
            it "parses with correct precedence if lower precedence on the right" $
                arithParser "1 * 3 + 2" `shouldBe` "((1*3)+2)" 
            it "parses with left associativity correctly" $
                arithParser "2 - 5 - 9 - 5 * 4" `shouldBe` "(((2-5)-9)-(5*4))"
            it "parses right associativity correctly" $
                arithParser "1 + 2 ^ 4 ^ 5" `shouldBe` "(1+(2^(4^5)))"
            it "parses no associativity correctly" $ do
                arithParser "1 + 2 < 3 * 4" `shouldBe` "((1+2)<(3*4))"
            it "parses brackets correctly" $ do
                arithParser "2 * ( 3+1 ) / (2-4)" `shouldBe` "((2*(3+1))/(2-4))"
            it "parses a complex expression correctly" $
                arithParser "1+7/2 > 2*3+4 && 2*3/7+1 < 5^(5-2 ) || 4*3 > 2"
                `shouldBe`
                "(((1+(7/2))>((2*3)+4))&&(((((2*3)/7)+1)<(5^(5-2)))||((4*3)>2)))"
