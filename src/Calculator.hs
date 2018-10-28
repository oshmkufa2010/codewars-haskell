module Calculator
(
    evaluate,
    expressParser
) where

import Text.Parsec

evaluate :: String -> Double
evaluate s = case parse (expressParser <* eof) "" s of
                Left e -> 0.1234
                Right r -> r
              
integerParser :: Parsec String () String
integerParser = do
    ingeters <- many1 digit `sepEndBy1` (many1 $ char ' ')
    return $ foldl1 (++) ingeters

mulOpParser :: Parsec String () (Double -> Double -> Double)
mulOpParser = char '+' >> return (+)

subOpParser :: Parsec String () (Double -> Double -> Double)
subOpParser = char '-' >> return (-)

timesOpParser :: Parsec String () (Double -> Double -> Double)
timesOpParser = char '*' >> return (*)

divOpParser :: Parsec String () (Double -> Double -> Double)
divOpParser = char '/' >> return (/) 

doubleParser :: Parsec String () Double
doubleParser = do
    int <- integerParser
    spaces
    doubt <- option ' ' $ char '.'

    value <-
        if doubt == '.' then do
            spaces
            float <- option "0" $ integerParser
            return $ int ++ "." ++ float
        else do
            return int

    return $ read value 

factorParser :: Parsec String () Double
factorParser = do
    spaces
    sym <- option '+' $ (char '+' <|> char '-')
    spaces
    value <- between (char '(') (char ')') expressParser <|> doubleParser
    spaces

    if sym == '-' then
        return $ value * (-1)
    else
        return value

termParser :: Parsec String () Double
termParser = chainl1 factorParser (timesOpParser <|> divOpParser) 

expressParser :: Parsec String () Double
expressParser = chainl1 termParser (mulOpParser <|> subOpParser)
