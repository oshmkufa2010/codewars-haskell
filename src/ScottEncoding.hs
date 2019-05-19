{-# LANGUAGE ScopedTypeVariables, Rank2Types #-}
module ScottEncoding where

import Prelude hiding (null, length, map, foldl, foldr, take, fst, snd, curry, uncurry, concat, zip, (++))

newtype SMaybe a = SMaybe { runMaybe :: forall b. b -> (a -> b) -> b }
newtype SList a = SList { runList :: forall b. b -> (a -> SList a -> b) -> b }
newtype SEither a b = SEither { runEither :: forall c. (a -> c) -> (b -> c) -> c }
newtype SPair a b = SPair { runPair :: forall c. (a -> b -> c) -> c }

toPair :: SPair a b -> (a,b)
toPair (SPair f) = f (,) 

fromPair :: (a,b) -> SPair a b
fromPair (a, b) = SPair (\abc -> abc a b)

fst :: SPair a b -> a
fst (SPair f) = f const 

snd :: SPair a b -> b
snd (SPair f) = f (flip const)

swap :: SPair a b -> SPair b a
swap (SPair f) = SPair $ \bac -> f $ \a b -> bac b a

curry :: (SPair a b -> c) -> (a -> b -> c)
curry spc = \a b -> spc $ SPair (\abc -> abc a b)

uncurry :: (a -> b -> c) -> (SPair a b -> c)
uncurry abc = \(SPair f) -> f abc

toMaybe :: SMaybe a -> Maybe a
toMaybe (SMaybe f) = f Nothing Just

fromMaybe :: Maybe a -> SMaybe a
fromMaybe Nothing = SMaybe const
fromMaybe (Just a) = SMaybe (\_ ab -> ab a)

isJust :: SMaybe a -> Bool
isJust (SMaybe f) = f False (const True)

isNothing :: SMaybe a -> Bool
isNothing (SMaybe f) = f True (const False)

catMaybes :: SList (SMaybe a) -> SList a
catMaybes = foldr (\(SMaybe f) la -> f la $ \a -> cons a la) $ SList const

toEither :: SEither a b -> Either a b
toEither (SEither f) = f Left Right

fromEither :: Either a b -> SEither a b
fromEither (Left a) = SEither (\ac _ -> ac a)
fromEither (Right b) = SEither (\_ bc -> bc b)

isLeft :: SEither a b -> Bool
isLeft (SEither f) = f (const True) (const False)

isRight :: SEither a b -> Bool
isRight (SEither f) = f (const False) (const True)

partition :: SList (SEither a b) -> SPair (SList a) (SList b)
partition = foldr (\(SEither fe) (SPair fp) -> SPair $ \lalbc -> fp $ \la lb -> fe (\a -> lalbc (cons a la) lb) (\b -> lalbc la (cons b lb))) $ SPair $ \abc -> abc (SList const) (SList const)

toList :: SList a -> [a]
toList (SList f) = f [] (\a sla -> a : toList sla)

fromList :: [a] -> SList a
fromList [] = SList const
fromList (a:as) = SList (\_ alab -> alab a (fromList as))

cons :: a -> SList a -> SList a
cons a la = SList (\_ alab -> alab a la)

concat :: SList a -> SList a -> SList a
concat (SList f) la2 = f la2 $ \a la -> cons a (concat la la2)

null :: SList a -> Bool
null (SList f) = f True (\_ _ -> False)

length :: SList a -> Int
length (SList f) = f 0 (\_ la -> length la + 1)

map :: (a -> b) -> SList a -> SList b
map ab (SList f) = SList $ \c blbc -> f c $ \a la -> blbc (ab a) (map ab la)

zip :: SList a -> SList b -> SList (SPair a b)
zip (SList fa) (SList fb) = SList $ \c ablabc -> fa c $ \a la -> fb c $ \b lb -> ablabc (SPair $ \abc -> abc a b) (zip la lb)

foldl :: (b -> a -> b) -> b -> SList a -> b
foldl bab b (SList f) = f b $ \a la -> foldl bab (bab b a) la

foldr :: (a -> b -> b) -> b -> SList a -> b
foldr abb b (SList f) = f b $ \a la -> abb a (foldr abb b la)

take :: Int -> SList a -> SList a
take i la@(SList f)
  | i <= 0 = SList const
  | otherwise = f (SList const) $ \a la' -> cons a $ take (i-1) la'
