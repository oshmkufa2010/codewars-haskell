module RegExpParser
       ( RegExp(..)
       , parseRegExp
       ) where

import Text.Parsec

data RegExp = Normal Char       -- ^ A character that is not in "()*|."
            | Any               -- ^ Any charater
            | ZeroOrMore RegExp -- ^ Zero or more occurances of the same regexp
            | Or RegExp RegExp  -- ^ A choice between 2 regexps
            | Str [RegExp]      -- ^ A sequence of regexps.
  deriving (Show, Eq)

charP :: Parsec String () RegExp
charP = Normal <$> noneOf "()*|."

anyP :: Parsec String () RegExp
anyP = char '.' >> return Any

factorP :: Parsec String () RegExp
factorP = between (char '(') (char ')') exprP <|> charP <|> anyP

termP :: Parsec String () RegExp
termP = do
  exp <- factorP
  option exp (char '*' >> return (ZeroOrMore exp))

termsP :: Parsec String () RegExp
termsP = do
  exps <- many1 termP
  case exps of
    [exp] -> return exp
    _ -> return $ Str exps

exprP :: Parsec String () RegExp
exprP = chainl1 termsP (char '|' >> return Or)

parseRegExp :: String -> Maybe RegExp
parseRegExp s = case parse (exprP <* eof) "" s of
  Left _ -> Nothing
  Right exp -> Just exp
