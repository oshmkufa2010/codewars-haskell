{-# LANGUAGE DeriveFunctor #-}

module OperatorParser
    ( OpTree(..)
    , Associativity(..)
    , op
    , foldTree
    , parseOperators
    , module Text.ParserCombinators.ReadP
    ) 
where

import Text.ParserCombinators.ReadP
import Control.Applicative hiding (many)
import Data.List

-- | Type for operator parse results. 'a' is the type of the operator, 'b'
-- | of the terms.
data OpTree a b = Op (OpTree a b) a (OpTree a b) 
                | Term b 
                deriving (Show, Eq, Functor)

-- | Type for specifying the assocativity of operators: left, right, or none.
data Associativity a = L a | R a | NoAssociativity a
                     deriving (Show, Eq, Functor)

-- | Transform an OpTree using the given function.
foldTree :: (a -> b -> b -> b) -> OpTree a b -> b
foldTree f (Op left op right) = f op (foldTree f left) (foldTree f right)
foldTree f (Term value) = value

-- | Return a parser such that: given 'op s a', if s matches, the parser 
-- | returns a.
op :: String -> a -> ReadP a
op s a = string s >> return a

-- | Accept two arguments: 
-- | (1) A list of type [Associativity [ReadP a]], which contains parsers for
-- | operators (ReadP a). Each item of type Associativity [ReadP a] contains
-- | a group of operator parsers of the same precedence and associativity; 
-- | these groups are listed in order of precedence (lowest to highest).
-- | (2) A parser for the terms.
-- | And return a parser for operator expressions that yields a parse tree. 
parseOperators :: [Associativity [ReadP a]] -> ReadP b -> ReadP (OpTree a b)
parseOperators associativities termParser = do
    let opParser ops = do
            skipSpaces
            op <- choice ops
            skipSpaces
            return (\left right -> Op left op right )
        
        parensParser = do
            char '('
            skipSpaces
            term <- parseOperators associativities termParser  
            skipSpaces
            char ')'
            return term
        
        folder asso nextLevelParser = 
            case asso of
                L ops -> chainl1 nextTermParser (opParser ops)
                R ops -> chainr1 nextTermParser (opParser ops)
                NoAssociativity ops -> nextTermParser <|> do
                    left <- nextTermParser
                    skipSpaces
                    op <- choice ops
                    skipSpaces
                    right <- nextTermParser
                    return $ Op left op right
                where
                    nextTermParser = parensParser <|> nextLevelParser
        in foldr folder (termParser >>= return . Term) associativities
