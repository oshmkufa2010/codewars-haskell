module Huffman
    ( frequencies
    , encode
    , decode
    , Bit (..)
    ) where

import Data.List
import qualified Data.Map as Map

data Bit = Z | O deriving (Eq, Show)

data HuffmanTree a = Leaf Int a | Node Int (HuffmanTree a) (HuffmanTree a) deriving (Show)

getWidght :: HuffmanTree a -> Int
getWidght (Leaf w _) = w 
getWidght (Node w _ _ ) = w

instance Eq (HuffmanTree a) where
  a == b = getWidght a == getWidght b

instance Ord (HuffmanTree a) where
  compare a b = compare (getWidght a) (getWidght b)

buildTree :: Ord a => [(a, Int)] -> Maybe (HuffmanTree a)
buildTree [] = Nothing
buildTree [_] = Nothing
buildTree freqs = Just $ head $ build $ sort $ map (\(x, w) -> Leaf w x) freqs
  where
    build :: [HuffmanTree a] -> [HuffmanTree a]
    build (x : y : trees) = build $ sort $ Node (getWidght x + getWidght y) x y : trees
    build trees = trees

buildEncodes :: Ord a => [(a, Int)] -> Maybe (Map.Map a [Bit])
buildEncodes freqs = buildTree freqs >>= \tree -> return $ Map.fromList $ buildEncode tree []
  where
    buildEncode :: HuffmanTree a -> [Bit] -> [(a, [Bit])]
    buildEncode (Leaf _ a) bs = [(a, bs)]
    buildEncode (Node _ left right) bs = (buildEncode left (bs ++ [Z])) ++ (buildEncode right (bs ++ [O]))

-- | Calculate symbol frequencies of a text.
frequencies :: Ord a => [a] -> [(a, Int)]
frequencies as = map (\xs -> (head xs, length xs)) $ group $ sort as

-- | Encode a sequence using the given frequencies.
encode :: Ord a => [(a, Int)] -> [a] -> Maybe [Bit]
encode freqs cs = do
  encodes <- buildEncodes freqs
  codes <- traverse (\c -> Map.lookup c encodes) cs
  return $ concat codes

-- | Decode a bit sequence using the given frequencies.
decode :: Ord a => [(a, Int)] -> [Bit] -> Maybe [a]
decode _ [] = Just []
decode freqs codes = do
  tree <- buildTree freqs
  build tree codes

  where
    go :: [Bit] -> HuffmanTree a -> Maybe (a, [Bit])
    go [] Node{} = Nothing
    go bs (Leaf _ a) = return (a, bs)
    go bs (Node _ left right) = do
      let (x : xs) = bs
      case x of
        Z -> go xs left
        O -> go xs right
    
    build :: HuffmanTree a -> [Bit] -> Maybe [a]
    build _ [] = Just []
    build tree bs = do
      result <- go bs tree
      case result of
        (a, []) -> return [a]
        (a, bs') -> do
          as <- build tree bs'
          return $ a : as
