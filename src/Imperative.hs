module Imperative (
  def, var, lit, while, (+=), (-=), (*=)
) where

import Data.Map
import Control.Monad.State

type Index = Int

newtype Variable = Variable Index
newtype Lit = Lit Integer
type Env = Map Index Integer
type ImpreativeExpr = State Env

class Value v where
  evalValue :: v -> Env -> Integer 

instance Value Variable where
  evalValue (Variable index) env = env ! index

instance Value Lit where
  evalValue (Lit value) _ = value

def :: Value v => ImpreativeExpr v -> Integer
def m = let (v, env) = runState m empty in evalValue v env

var :: Integer -> ImpreativeExpr Variable
var v = do
  env <- get
  let index = length env
  put $ insert index v env
  return $ Variable index

lit :: Integer -> Lit
lit = Lit

while :: Value v => v -> (Integer -> Bool) -> ImpreativeExpr () -> ImpreativeExpr () 
while r f act = do
  env <- get
  let value = evalValue r env
  when (f value) $ do
    let (_, env') = runState act env
    put env'
    while r f act

assignWithOp :: Value v => (Integer -> Integer -> Integer) -> Variable -> v -> ImpreativeExpr ()
assignWithOp op (Variable index) b = do
  env <- get
  let
    val1 = env ! index
    val2 = evalValue b env
    env' = insert index (op val1 val2) env
    in put env'

(+=), (-=), (*=) :: Value v => Variable -> v -> ImpreativeExpr ()

(+=) = assignWithOp (+)
(-=) = assignWithOp (-)
(*=) = assignWithOp (*)
