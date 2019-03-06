{-# LANGUAGE LambdaCase #-}

module ApplicativeParser where

    import Data.Char
    import Data.List

    import Prelude hiding (fmap)
    
    -- | An ambiguous parser.
    newtype Parser a = P { unP :: String -> [(String, a)] }

    -- | Change the result of a parser.
    pmap :: (a -> b) -> Parser a -> Parser b
    pmap f (P p) = P $ map (\(s', a) -> (s', f a)) . p 
    
    -- | Operator version of 'pmap'.
    (<#>) :: (a -> b) -> Parser a -> Parser b
    (<#>) = pmap
    
    -- | Parse a value and replace it.
    (<#) :: a -> Parser b -> Parser a
    (<#) = pmap . const
    
    infixl 4 <#>
    infixl 4 <#

    anyChar :: Parser Char
    anyChar = P $ \case
        [] -> []
        (x:xs) -> [(xs, x)]
    
    -- | Parse a character only when a predicate matches.
    predP :: (Char -> Bool) -> Parser Char
    predP f = P $ \case
        [] -> []
        (x:xs) -> [(xs, x) | f x]

    -- | Succeed only when parsing the given character.
    charP :: Char -> Parser Char
    charP = predP . (==)
    
    -- | Inject a value into an identity parser.
    inject :: a -> Parser a
    inject a = P $ \s -> [(s, a)]
    
    -- | Given a parser with a function value and another parser, parse the function
    -- first and then the value, return a parser which applies the function to the
    -- value.
    (<@>) :: Parser (a -> b) -> Parser a -> Parser b
    P pf <@> P pa = P $ \s ->
        do
            (s', f) <- pf s
            (s'', a) <- pa s'

            return (s'', f a)

    
    (<@) :: Parser a -> Parser b -> Parser a
    pa <@ pb = const <#> pa <@> pb
    
    (@>) :: Parser a -> Parser b -> Parser b
    pa @> pb = (\a b -> b) <#> pa <@> pb
    
    infixl 4 <@
    infixl 4 @>
    infixl 4 <@>
    
    -- | Parse a whole string.
    stringP :: String -> Parser String
    stringP = foldr (\c ps -> (:) <#> charP c <@> ps) $ inject ""
    
    -- | Construct a parser that never parses anything.
    emptyP :: Parser a
    emptyP = P $ const []
    
    -- | Combine two parsers: When given an input, provide the results of both parser run on the input.
    (<<>>) :: Parser a -> Parser a -> Parser a
    pl <<>> pr = P $ \s -> unP pl s ++ unP pr s
    
    infixl 3 <<>>
    
    -- | Apply the parser zero or more times.
    many :: Parser a -> Parser [a]
    many p = some p <<>> inject [] 
    
    -- | Apply the parser one or more times.
    some :: Parser a -> Parser [a]
    some p = (:) <#> p <@> many p
   
    
    -- | Apply a parser and return all ambiguous results, but only those where the input was fully consumed.
    runParser :: Parser a -> String -> [a]
    runParser p cs = map snd $ filter ((== "") . fst) $ unP p cs
    
    -- | Apply a parser and only return a result, if there was only one unambiguous result with output fully consumed.
    runParserUnique :: Parser a -> String -> Maybe a
    runParserUnique p cs = case runParser p cs of
        [a] -> Just a
        _ -> Nothing
    
    -- | Kinds of binary operators.
    data BinOp = AddBO | MulBO deriving (Eq, Show)
    
    -- | Some kind of arithmetic expression.
    data Expr = ConstE Int
              | BinOpE BinOp Expr Expr
              | NegE Expr
              | ZeroE
              deriving (Eq, Show)
    
    evalExpr :: Expr -> Int
    evalExpr (ConstE i) = i
    evalExpr (BinOpE op e1 e2) =
        let 
            i1 = evalExpr e1
            i2 = evalExpr e2
            in 
                case op of
                    AddBO -> i1 + i2
                    MulBO -> i1 * i2
    evalExpr (NegE e) = - evalExpr e
    evalExpr ZeroE = 0
    
    -- | Parse arithmetic expressions, with the following grammar:
    --
    --     expr         ::= const | binOpExpr | neg | zero
    --     const        ::= int
    --     binOpExpr    ::= '(' expr ' ' binOp ' ' expr ')'
    --     binOp        ::= '+' | '*'
    --     neg          ::= '-' expr
    --     zero         ::= 'z'
    -- 

    parseExpr :: String -> Maybe Expr
    parseExpr = let
        exprP = constP <<>> opExprP <<>> negP <<>> zeroP
        constP = pmap (ConstE . (\s -> read s :: Int)) $ some $ predP isNumber 

        opPf _ l _ op _ r _ = BinOpE op l r

        opExprP = opPf <#> charP '(' <@> exprP <@> charP ' ' <@> opP <@> charP ' ' <@> exprP <@> charP ')'
        opP = charP '+' @> inject AddBO <<>> charP '*' @> inject MulBO

        negP = charP '-' @> pmap NegE exprP

        zeroP = charP 'z' @> inject ZeroE
        in runParserUnique exprP
