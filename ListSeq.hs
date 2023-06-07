module ListSeq where

import Par
import Seq

contract :: (a -> a -> a) -> [a] -> [a]
contract _ [] = []
contract _ [x] = [x]
contract f (x:y:s) = let (z, zs) = (f x y) ||| contract f s in (z:zs)

expand :: (a -> a -> a) -> [a] -> [a] -> [a]
expand f ps s = expand' f ps s
              where
                expand' f rs [] = rs
                expand' f rs [x] = rs
                expand' f (r:rs) (s1:s2:ss) = r:(f r s1):expand' f rs ss

instance Seq [] where
  emptyS = []

  singletonS x = [x]

  lengthS = length

  nthS xs i = xs !! i

  tabulateS f n = mapS f [0..(n-1)]

  mapS = map

  filterS = filter

  appendS = (++)

  takeS s n = take n s

  dropS s n = drop n s

  showtS [] = EMPTY
  showtS [x] = ELT x
  showtS xs = let n = (div (length xs) 2) in NODE (take n xs) (drop n xs)

  showlS [] = NIL
  showlS (x:xs) = CONS x xs

  joinS = foldr (++) []

  --reduceS :: (a -> a -> a) -> a -> [a] -> a
  reduceS f e xs = reduceS' f e xs (length xs)
          where
            reduceS' f e xs n | n == 0 = e
                              | n == 1 = f e (nthS xs 0)
                              | otherwise = let (q,k) = (n `div` 2, n `mod` 2)
                                                n' = q + k
                                                xs' = contract f xs
                                            in reduceS' f e xs' n'

  scanS f e xs | n == 0 = (emptyS, e)
              | n == 1 = let x = nthS xs 0 in (singletonS e, f e x)
              | otherwise = let (s',r) = scanS f e $ contract f xs in (expand f s' xs, r)
            where n = lengthS xs

  fromList = id
