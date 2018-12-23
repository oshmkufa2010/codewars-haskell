module Imperative (
  def, var, lit, while, (+=), (-=), (*=)
) where

import Data.Map
import Control.Monad.State
data Variable = Index Int | Lit Integer
type ImpreativeMonad = State (Map Int Integer)

def :: ImpreativeMonad Variable -> Integer
def m = case v of
  Index index -> env ! index
  Lit value -> value
  where (v, env) = runState m empty

var :: Integer -> ImpreativeMonad Variable
var v = do
  env <- get
  let index = length env
  put $ insert index v env
  return $ Index index

lit :: Integer -> Variable
lit = Lit

while :: Variable -> (Integer -> Bool) -> ImpreativeMonad () -> ImpreativeMonad () 
while r f act = do
  env <- get
  let
    value = case r of
      Index i -> env ! i
      Lit v -> v
    in
      when (f value) $
        let (_, env') = runState act env in put env' >> while r f act
        

assignWithOp :: (Integer -> Integer -> Integer) -> Variable -> Variable -> ImpreativeMonad ()
assignWithOp op (Index index) b = do
  env <- get
  let
    val1 = env ! index
    val2 = case b of
      Index i -> env ! i
      Lit v -> v
    env' = insert index (op val1 val2) env
    in put env'

(+=), (-=), (*=) :: Variable -> Variable -> ImpreativeMonad ()

(+=) = assignWithOp (+)
(-=) = assignWithOp (-)
(*=) = assignWithOp (*)
