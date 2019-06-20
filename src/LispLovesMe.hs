{-# LANGUAGE LambdaCase #-}

module LispLovesMe where

import Text.Parsec
import Control.Monad (guard)

data AST = I32 Int
          | Sym String
          | Nul
          | Err
          | Lst [AST]
          | Boo Bool
          | Nod AST [AST]
          deriving (Eq, Show)
--

data Result = ListResult [Result]
  | IntResult Int
  | SymResult String
  | NulResult
  | BoolResult Bool
  deriving (Show, Eq)

foldAST :: AST -> Maybe Result
foldAST (I32 i) = Just $ IntResult i
foldAST (Sym str) = Just $ SymResult str
foldAST Nul = Just NulResult 
foldAST Err = Nothing
foldAST (Lst asts) = ListResult <$> traverse foldAST asts
foldAST (Boo b) = Just $ BoolResult b
foldAST (Nod (Sym op) asts) = do
  results <- traverse foldAST asts 

  case op of
    _ | op `elem` ["+", "-", "*", "/"] -> do
      guard $ length results >= 2

      values <- fetchInts results
      
      case op of
        "+" -> return $ IntResult $ foldl1 (+) values
        "-" -> return $ IntResult $ foldl1 (-) values
        "*" -> return $ IntResult $ foldl1 (*) values
        "/" -> return $ IntResult $ foldl1 div values
        _ -> Nothing

    _ | op `elem` ["^", ">", "<", ">=", "<=", "==", "!="] -> do
      [a, b] <- fetchInts results

      case op of
        "^" -> return $ IntResult $ a ^ b
        ">" -> return $ BoolResult $  a > b
        "<" -> return $ BoolResult $ a < b
        ">=" -> return $ BoolResult $ a >= b
        "<=" -> return $ BoolResult $ a <= b
        "==" -> return $ BoolResult $ a == b
        "!=" -> return $ BoolResult $ a /= b
        _ -> Nothing
    
    "!" -> do
      [b] <- fetchBools results
      return $ BoolResult $ not b
    
    "list" -> return $ ListResult results 

    "size" -> do
      let [ListResult list] = results
      return $ IntResult $ length list

    "reverse" -> do
      let [ListResult list] = results
      return $ ListResult $ reverse list

    ".." -> do
      [l, r] <- fetchInts results
      return $ ListResult $ fmap IntResult [l..r]
    
    "if" -> do
      BoolResult a : bc <- return results
      case bc of 
        [b] -> return $ if a then b else NulResult
        [b, c] -> return $ if a then b else c
        _ -> Nothing
   
    _ -> Nothing
    
  where
    fetchInts = traverse $ \case
      IntResult i -> Just i
      _ -> Nothing

    fetchBools = traverse $ \case
      BoolResult b -> Just b
      _ -> Nothing

foldAST _ = Nothing

resultToAst :: Result -> AST
resultToAst (ListResult rs) = Lst $ fmap resultToAst rs
resultToAst (IntResult i) = I32 i
resultToAst (SymResult str) = Sym str
resultToAst NulResult = Nul
resultToAst (BoolResult b) = Boo b
    
lispPretty :: String -> Maybe String
lispPretty s = 
  case parse (between spaces spaces exprParser) "" s of
    Right ast -> astPretty ast
    _ -> Nothing

  where
    astPretty :: AST -> Maybe String
    astPretty ast = 
      case ast of
        I32 i -> return $ show i
        Sym str -> return str
        Nul -> return "null"
        Err -> Nothing
        Lst asts -> return "undefined"
        Boo b -> return $ if b then "true" else "false"
        Nod opAst asts -> do
          opStr <- astPretty opAst
          args <- traverse astPretty asts
          return $ case args of
            [] -> "(" ++ opStr ++ ")"
            _ -> let argsStr = foldr1 (\arg str -> arg ++ " " ++ str) args in "(" ++ opStr ++ " " ++ argsStr ++ ")"

lispEval :: String -> Maybe AST
lispEval s = case parse (between spaces spaces exprParser) "" s of
    Left _ -> Nothing
    Right ast -> case foldAST ast of
      Just result -> Just $ resultToAst result
      Nothing -> Just Err

nodeParser :: Parsec String () AST
nodeParser = do
  char '('
  spaces
  exprs <- exprParser `sepEndBy` many space
  spaces
  char ')'

  case exprs of
    [] -> return Nul
    (x:xs) -> return $ Nod x xs

-- parse I32, Sym, Null, Boo
atomParser :: Parsec String () AST
atomParser = do
  str <- many1 $ noneOf " ,\n\t\r()"
  
  case str of
    _ | all (\c -> c `elem` ['0'..'9']) str -> return $ I32 (read str :: Int)
    "null" -> return Nul
    "true" -> return $ Boo True 
    "false" -> return $ Boo False
    _ -> return $ Sym str
    
exprParser :: Parsec String () AST
exprParser = atomParser <|> nodeParser
