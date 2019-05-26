{-# LANGUAGE RankNTypes #-}

module FinallyTaglessInterpreter where

import Prelude hiding (and, or)

class Language r where
  here   :: r (a, h) a
  before :: r h a -> r (any, h) a
  lambda :: r (a, h) b -> r h (a -> b)
  apply  :: r h (a -> b) -> (r h a -> r h b)
  
  loop   :: r h (a -> a) -> r h a
  
  int    :: Int -> r h Int
  add    :: r h Int -> r h Int -> r h Int
  down   :: r h Int -> r h Int    -- \x -> x - 1
  up     :: r h Int -> r h Int    -- \x -> x + 1
  mult   :: r h Int -> r h Int -> r h Int
  gte    :: r h Int -> r h Int -> r h Bool -- greater than or equal
  
  bool   :: Bool -> r h Bool
  and    :: r h Bool -> r h Bool -> r h Bool
  or     :: r h Bool -> r h Bool -> r h Bool
  neg    :: r h Bool -> r h Bool
  
  ifte   :: r h Bool -> r h a -> r h a -> r h a -- if true then return left term, else return right term

type Term a = forall r h . Language r => r h a

instance Language (->) where
  here = fst
  before ha = \(_, h) -> ha h
  lambda ahb = \h -> \a -> ahb (a, h)
  apply hab = \ha -> \h -> hab h (ha h)

  loop haa = apply haa (loop haa)

  int i = const i
  add hi1 hi2 = \h -> (hi1 h) + (hi2 h)
  down hi = \h -> (hi h) - 1
  up hi = \h -> (hi h) + 1
  mult hi1 hi2 = \h -> (hi1 h) * (hi2 h)
  gte hi1 hi2 = \h -> (hi1 h) >= (hi2 h) 
  
  bool b = const b
  and hb1 hb2 = \h -> (hb1 h) && (hb2 h)
  or hb1 hb2 = \h -> (hb1 h) || (hb2 h)
  neg hb = \h -> not (hb h)

  ifte hb ha1 ha2 = \h -> if hb h then ha1 h else ha2 h

  
interpret :: Term a -> a
interpret t = t ()