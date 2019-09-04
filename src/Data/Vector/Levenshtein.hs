module Data.Vector.Levenshtein where

import qualified Data.Vector                   as V
import           Data.Vector                    ( Vector
                                                , (//)
                                                , (!)
                                                )
import           Data.Vector.Mutable            ( write )

type Matrix a = Vector (Vector a)

-- | /O(1) or O(n)/ Update an index using write, in place if safe to do so.
poke :: Vector a -> Int -> a -> Vector a
poke v ix a = V.modify (\vec -> write vec ix a) v

-- | /O(1) or O(n)/ An element update on a Matrix.
mutate :: Matrix a -> (Int, Int) -> a -> Matrix a
mutate m (y, x) a = poke m y (poke (m ! y) x a)

-- | /O(n^2)/ Generate an empty MxN matrix.
empty2D :: Int -> Int -> Matrix Int
empty2D m n = V.replicate m (V.replicate n 0)

-- | /O(n^2)/ Construct the initial Levenshtein matrix.
initial :: Int -> Int -> Matrix Int
initial m n = V.fromList [V.fromList [rules m' n' | m' <- [0..m]] | n' <- [0..n]]
  where rules m n
          | m == 0    = n
          | n == 0    = m
          | otherwise = 0

-- | /O(1)/ Index a matrix.
at :: Matrix a -> (Int, Int) -> a
at mat (y, x) = (mat ! y) ! x

-- | Wagner-Fischer Levenshtein distance function.
distance' :: Eq a => [a] -> [a] -> Matrix Int
distance' s t = go (initial m n) 1
  where
    go mat i
      | i == (n + 1)    = mat
      | otherwise = go (go' mat i 1) $ succ i
    go' mat i j
      | j == (m + 1)    = mat
      | otherwise = go' (change mat i j $ cost (s !! (i - 1)) (t !! (j - 1))) i (succ j)
    n = length s
    m = length t

distance :: Eq a => [a] -> [a] -> Int
distance s t = V.last $ V.last $ distance' s t

-- | Mutate a Matrix according to Levenshtein rules.
change :: Matrix Int -> Int -> Int -> Int -> Matrix Int 
change mat i j c = mutate mat (i, j) lowest
  where lowest = minimum [mat `at` (i - 1, j) + 1, mat `at` (i, j - 1) + 1, mat `at` (i - 1, j - 1) + c]

-- | /O(1)/ Calculate substitution cost.
cost :: Eq a => a -> a -> Int
cost a b | a == b    = 0
         | otherwise = 1
