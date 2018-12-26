{-# LANGUAGE LambdaCase #-}

module Transpiler where

  import Data.Char
  import Data.List
  import Control.Monad
  import Control.Applicative
  import Control.Monad.State
  
  alpha :: String
  alpha = ['a'..'z'] ++ ['A'..'Z']
  
  digit :: String
  digit = ['0' .. '9']
  
  tokenize :: String -> [String]
  tokenize [] = []
  tokenize xxs@(c : cs)
    | c == '-' && head cs == '>' = "->" : tokenize (tail cs)
    | c `elem` "(){}," = [c] : tokenize cs
    | not (null s) = s : tokenize ss
    | otherwise = tokenize cs
    where
      (s, ss) = span (`elem` "_" ++ alpha ++ digit) xxs
  
  -----------------------------------------------------
  -------------- your parser combinator ---------------
  -----------------------------------------------------
  
  type Parser val = StateT [String] [] val

  predP :: (String -> Bool) -> Parser String
  predP f = StateT $ \case
      [] -> []
      (token:tokens) -> [(token, tokens) | f token]
  
  parseToken :: String -> Parser String 
  parseToken t = predP (== t) 

  (<~>) :: Alternative a => a b -> a b -> a b
  (<~>) = flip (<|>)

  parseCode :: Parser a -> String -> Either String a
  parseCode m s =
    case filter ((== []) . snd) result of
      [(res, _)] -> Right res
      _           -> Left "Hugh?"
    where result = runStateT m (tokenize s)

  option :: val -> Parser val -> Parser val
  option x p = p <|> pure x
  
  functionP :: Parser String
  functionP = do
    funcName <- expressionP

    funcBody <- paramLambdaP <|> do l <- lambdaP; return ("(" ++ l ++ ")")

    return $ funcName ++ funcBody
  
  paramLambdaP :: Parser String
  paramLambdaP = do
    parseToken "("
    params <- option [] $ do param <- parametersP; return [param]
    parseToken ")"
    lambda <- option [] $ do l <- lambdaP; return [l]
    return $ "(" ++ intercalate "," (params ++ lambda) ++ ")"

  variableP :: Parser String
  variableP = predP $ \token -> head token `elem` ('_' : alpha)
  
  numberP :: Parser String
  numberP = predP $ all (\c -> c `elem` ['0'..'9'])

  expressionP :: Parser String
  expressionP = variableP <|> numberP <|> lambdaP

  parametersP :: Parser String
  parametersP = do
    param <- expressionP
    params <- option [] $ parseToken "," >> do p <- parametersP; return [p]
    return $ intercalate "," (param : params)

  lambdaparamP :: Parser String
  lambdaparamP = do
    param <- variableP <|> numberP
    params <- option [] (parseToken "," >> do p <- lambdaparamP; return [p])
    return $ intercalate "," (param : params)
  
  lambdastmtP :: Parser String
  lambdastmtP = do
    stmt <- variableP <|> numberP
    stmts <- many (variableP <|> numberP)
    return $ concatMap (++ ";") (stmt : stmts)
  
  lambdaP :: Parser String
  lambdaP = do
    parseToken "{"
    params <- option "" $ lambdaparamP <* parseToken "->"
    stmts <- option "" lambdastmtP
    parseToken "}"
    return $ "(" ++ params ++ ")" ++ "{" ++ stmts ++ "}"
  
  transpile :: String -> Either String String
  transpile = parseCode functionP
