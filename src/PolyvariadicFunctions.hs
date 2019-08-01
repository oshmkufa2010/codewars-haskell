{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleInstances, FlexibleContexts, FunctionalDependencies #-}

module PolyvariadicFunctions where

-- `polyAdd` sums its arguments, all `Int`s.
class PolyAdd r where
  buildPolyAdd :: Int -> r
  
instance PolyAdd Int where
  buildPolyAdd = id

instance (a ~ Int, PolyAdd r) => PolyAdd (a -> r) where
  buildPolyAdd x y = buildPolyAdd (x + y)

polyAdd :: PolyAdd r => r
polyAdd = buildPolyAdd 0

-- `polyList` turns its arguments into a list, polymorphically.
class PolyList a r | r -> a where
  buildPolyList :: [a] -> r

instance PolyList a [a] where
  buildPolyList = id

instance PolyList a r => PolyList a (a -> r) where
  buildPolyList xs y = buildPolyList $ xs ++ [y]

polyList :: PolyList a r => r
polyList = buildPolyList []

-- `polyWords` turns its arguments into a spaced string.
class PolyWords r where
  buildPolyWords :: Int -> String -> r

instance PolyWords String where
  buildPolyWords _ str = str

instance (PolyWords r) => PolyWords (String -> r) where
  buildPolyWords pos x y = buildPolyWords (pos + 1) $ if pos == 0 then y else x ++ " " ++ y

polyWords :: PolyWords r => r
polyWords = buildPolyWords 0 ""
