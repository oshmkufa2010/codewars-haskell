{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts #-}

module Postfix where

data Push = Push
data Add = Add
data End = End

class Forth r where
  build :: [Int] -> r

instance (a ~ Int) => Forth (End -> a) where
  build stack _ = head stack

instance Forth r => Forth (Add -> r) where
  build (n1 : n2 : stack) _ = build $ (n1 + n2) : stack

instance (Forth r, a ~ Int) => Forth (Push -> a -> r) where
  build stack _ n = build $ n : stack

begin :: Forth r => r
begin = build []
end = End
add = Add
push = Push
