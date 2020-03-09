module SimpleInteractiveInterpreter where

import Text.Parsec
import qualified Data.Map as M
import Data.Bifunctor (first)
import Control.Monad (foldM)
import Control.Monad.State

type Parser a = Parsec String () a

data Fn = Fn String [String] Expr deriving (Show)

type ResultValue = Integer

data Expr = Var String
  | Lit ResultValue
  | Assignment String Expr
  | FnCall String [Expr]
  | Add Expr Expr
  | Minus Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  | Mod Expr Expr
  deriving (Show)

data Ast = ExprNode Expr | FnNode Fn | EmptyNode deriving (Show)

spaces1 :: Parser [Char]
spaces1 = many1 space

fnP :: Parser Fn
fnP = do
  string "fn"
  spaces1
  fnName <- idenP
  spaces1
  vars <- idenP `sepEndBy` spaces1
  string "=>"
  spaces1
  expr <- exprP
  return $ Fn fnName vars expr

idenP :: Parser String
idenP = do
  x <- letter <|> char '_'
  xs <- many $ char '_' <|> letter <|> digit
  return $ x : xs

varP :: Parser Expr
varP = fmap Var idenP 

numP :: Parser Expr
numP = do
  int <- many1 digit
  dot <- optionMaybe $ char '.'
  case dot of
    Just _ -> many1 digit >>= \double -> return $ Lit $ read $ int ++ "." ++ double
    Nothing -> return $ Lit $ read int

assignP :: Parser Expr
assignP = do
  var <- idenP
  spaces
  char '='
  spaces
  expr <- exprP
  return $ Assignment var expr

fnCallP :: Parser Expr
fnCallP = do
  fnName <- idenP
  spaces1
  exprs <- (try assignP <|> varP <|> numP <|> between (char '(') (char ')') exprP) `sepEndBy1` spaces1
  return $ FnCall fnName exprs

exprP :: Parser Expr
exprP = termP `chainl1` ((char '+' >> return Add) <|> (char '-' >> return Minus))

termP :: Parser Expr
termP = factorP `chainl1` ((char '*' >> return Mult) <|> (char '/' >> return Div) <|> (char '%' >> return Mod))

factorP :: Parser Expr
factorP = between spaces spaces $ numP <|> try assignP <|> try fnCallP <|> try varP <|> between (char '(') (char ')') exprP

astP :: Parser Ast
astP = spaces >> (try (fmap FnNode fnP) <|> fmap ExprNode exprP <|> (spaces >> return EmptyNode)) <* eof

data SymTabVal = Value ResultValue | FnDef Fn deriving (Show)
type Env = M.Map String SymTabVal
type Interpreter = Env
type Result = Maybe ResultValue

newInterpreter :: Interpreter
newInterpreter = M.empty

input :: String -> Interpreter -> Either String (Result, Interpreter)
input command env = do
  ast <- first show $ parse astP "" command
  case ast of
    FnNode fn@(Fn fnName args body) -> case M.lookup fnName env of
      Just (Value _) -> Left (fnName ++ " has been defined")
      _ ->
        if any (`notElem` args) $ listVars body then
          Left "error"
        else
          Right (Nothing, M.insert fnName (FnDef fn) env)
    ExprNode expr -> fmap (first Just) $ runStateT (evalExpr expr) env
    EmptyNode -> Right (Nothing, env)

evalExpr :: Expr -> StateT Env (Either String) ResultValue
evalExpr expr = do
  env <- get
  case expr of
    Var varName ->
      case M.lookup varName env of
        Nothing -> lift $ Left $ varName ++ " undefined"
        Just (Value val) -> return val
        Just (FnDef (Fn _ [] body)) -> evalExpr body
        Just (FnDef _) -> lift $ Left $ varName ++ " isn't a variable"

    Lit val -> return val

    Assignment varName expr -> case M.lookup varName env of
      Just (FnDef _) -> lift $ Left (varName ++ " has been defined as a function")
      _ -> do 
        val <- evalExpr expr
        env' <- get
        put $ M.insert varName (Value val) env'
        return val

    FnCall fnName exprs -> case M.lookup fnName env of
      Nothing -> lift $ Left $ fnName ++ " undefined"
      Just (Value _) -> lift $ Left $ fnName ++ " isn't a function"
      Just (FnDef fn@(Fn _ args body)) ->
        do
          argVals <- evalArgs exprs
          if length argVals /= length args then
            lift $ Left $ fnName ++ " args error"
          else
            do
              put $ M.fromList $ zip args $ fmap Value argVals
              evalExpr body

    Add expr1 expr2 -> liftM (+) (evalExpr expr1) `ap` evalExpr expr2
    Minus expr1 expr2 -> liftM (-) (evalExpr expr1) `ap` evalExpr expr2
    Mult expr1 expr2 -> liftM (*) (evalExpr expr1) `ap` evalExpr expr2
    Div expr1 expr2 -> liftM div (evalExpr expr1) `ap` evalExpr expr2
    Mod expr1 expr2 -> liftM mod (evalExpr expr1) `ap` evalExpr expr2

evalArgs :: [Expr] -> StateT Env (Either String) [ResultValue]
evalArgs [] = return []
evalArgs (expr : exprs) = do
  env <- get
  case expr of
    Var varName -> case M.lookup varName env of
      Nothing -> lift $ Left $ varName ++ " undefined"
      Just (Value val) -> liftM (val:) $ evalArgs exprs
      Just (FnDef fn@(Fn fnName argNames body)) ->
        if length argNames > length exprs then 
          lift $ Left $ "args of " ++ varName ++ " is error"
        else
          do
            vals <- mapM evalExpr $ take (length argNames) exprs
            put $ M.fromList $ zip argNames $ fmap Value vals
            v <- evalExpr body
            put env
            vs <- evalArgs $ drop (length argNames) exprs
            return $ v : vs
    _ -> liftM (:) (evalExpr expr) `ap` evalArgs exprs

listVars :: Expr -> [String]
listVars (Var varName) = [varName]
listVars (Lit _) = []
listVars (Assignment _ expr) = listVars expr
listVars (FnCall fnName exprs) = fnName : foldMap listVars exprs
listVars (Add expr1 expr2) = listVars expr1 ++ listVars expr2
listVars (Minus expr1 expr2) = listVars expr1 ++ listVars expr2
listVars (Mult expr1 expr2) = listVars expr1 ++ listVars expr2
listVars (Div expr1 expr2) = listVars expr1 ++ listVars expr2
listVars (Mod expr1 expr2) = listVars expr1 ++ listVars expr2
