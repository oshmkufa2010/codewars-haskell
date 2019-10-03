module TinyThreePassCompiler where

import Text.Parsec
import qualified Data.Map as M

{-- 

function   ::= '[' arg-list ']' expression

arg-list   ::= /* nothing */
             | variable arg-list

expression ::= term
             | expression '+' term
             | expression '-' term

term       ::= factor
             | term '*' factor
             | term '/' factor

factor     ::= number
             | variable
             | '(' expression ')'

--}

data AST = Imm Int
          | Arg Int
          | Add AST AST
          | Sub AST AST
          | Mul AST AST
          | Div AST AST
          deriving (Eq, Show)

type Parser a = Parsec String () a

type Env = M.Map String Int

alpha :: String
alpha = ['a'..'z'] ++ ['A'..'Z']

functionP :: Parser AST
functionP = do
  char '['
  spaces
  env <- argListP
  spaces
  char ']'
  spaces
  exprP env

argListP :: Parser Env
argListP = do
  vars <- sepEndBy varP (many1 space)
  return $ M.fromList $ zip vars [0..length vars - 1]

exprP :: Env -> Parser AST
exprP env = chainl1 (termP env) ((char '+' >> return Add) <|> (char '-' >> return Sub))

termP :: Env -> Parser AST
termP env = chainl1 (between spaces spaces $ factorP env) ((char '*' >> return Mul) <|> (char '/' >> return Div))

factorP :: Env -> Parser AST
factorP env = (Imm <$> numP) <|> (varP >>= \var -> return $ Arg $ env M.! var) <|> do
  char '('
  spaces
  expr <- exprP env
  spaces
  char ')'
  return expr

varP :: Parser String
varP = many1 $ oneOf alpha

numP :: Parser Int
numP = many1 (oneOf ['0'..'9']) >>= \n -> return (read n :: Int)

compile :: String -> [String]
compile = pass3 . pass2 . pass1

pass1 :: String -> AST
pass1 src = case parse functionP "" src of
  Right ast -> ast

pass2 :: AST -> AST
pass2 ast = 
  case ast of
    Add ast1 ast2 -> optImm (+) Add ast1 ast2
    Sub ast1 ast2 -> optImm (-) Sub ast1 ast2
    Mul ast1 ast2 -> optImm (*) Mul ast1 ast2
    Div ast1 ast2 -> optImm div Div ast1 ast2
    _ -> ast
  where
    optImm :: (Int -> Int -> Int) -> (AST -> AST -> AST) -> AST -> AST -> AST 
    optImm f astCons ast1 ast2 = 
      let
        ast1' = pass2 ast1
        ast2' = pass2 ast2
        in
          case (ast1', ast2') of
            (Imm v1, Imm v2) -> Imm $ f v1 v2
            _ -> astCons ast1' ast2'

{-
"IM n"     // load the constant value n into R0
"AR n"     // load the n-th input argument into R0
"SW"       // swap R0 and R1
"PU"       // push R0 onto the stack
"PO"       // pop the top value off of the stack into R0
"AD"       // add R1 to R0 and put the result in R0
"SU"       // subtract R1 from R0 and put the result in R0
"MU"       // multiply R0 by R1 and put the result in R0
"DI"       // divide R0 by R1 and put the result in R0
-}

pass3 :: AST -> [String]
pass3 ast = case ast of
  Add ast1 ast2 -> genBinIns "AD" ast1 ast2
  Sub ast1 ast2 -> genBinIns "SU" ast1 ast2
  Mul ast1 ast2 -> genBinIns "MU" ast1 ast2
  Div ast1 ast2 -> genBinIns "DI" ast1 ast2
  Imm v -> ["IM " ++ show v]
  Arg n -> ["AR " ++ show n]
  where
    genBinIns op ast1 ast2 =  pass3 ast1 ++ ["PU"] ++ pass3 ast2 ++ ["SW", "PO", op]
