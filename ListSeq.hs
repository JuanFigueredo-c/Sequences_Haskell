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
                expand' f (r:rs) (s1:s2:ss) = let (r', rs') = (f r s1) |||
                                                              expand' f rs ss
                                              in r:r':rs'

instance Seq [] where
  emptyS = []

  singletonS x = [x]

  lengthS = length

  nthS xs i = xs !! i

  tabulateS f n = mapS f [0..(n-1)]

  mapS f [] = []
  mapS f (x:xs) = let (y,ys) = f x ||| mapS f xs in y:ys

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

  reduceS f e []  = e
  reduceS f e [x] = f e x
  reduceS f e xs  = let xs' = contract f xs in reduceS f e xs'

  scanS f e []  = (emptyS, e)
  scanS f e [x] = (singletonS e, f e x)
  scanS f e xs  = let (s',r) = scanS f e $ contract f xs in (expand f s' xs, r)

  fromList = id
