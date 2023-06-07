module ListSeq where

import Par
import Seq

contract :: (a -> a -> a) -> [a] -> [a]
contract _ [x] = [x]
contract f (x:y:s) = let (z, zs) = (f x y) ||| contract f s in (z:zs)

expand :: (a -> a -> a) -> ([a], a) -> [a] -> ([a], a)
expand f (ps, t) s = (tabulateS g (lengthS s), t)
                where
                  g i | even i = ps ! (div i 2)
                      | otherwise = let (x, y) = ps ! (div i 2) |||
                                                 s  ! (i - 1)
                                    in f x y

instance Seq [] where
  emptyS = []

  singletonS x = [x]

  lengthS = length

  nthS xs i = xs ! i

  tabulateS f n = mapS f [0..(n-1)]

  mapS f [] = []
  mapS f (x:xs) = let (y, ys) = f x ||| mapS f xs -- TODO chequear si paralelizar mejora el O
                  in y:ys

  filterS = filter

  appendS = (++)

  takeS = take

  dropS = drop

  showtS [] = EMPTY
  showtS [x] = ELT x
  showtS xs = let n = (div (length xs) 2) in NODE (take xs n) (drop xs n)

  showlS [] = NIL
  showlS (x:xs) = CONS x xs

  joinS = foldr (++) []

  reduceS :: (a -> a -> a) -> a -> [a] -> a
  reduceS g e xs = reduceS' g e xs (length xs)
          where
            reduceS' g e xs n | n == 0 = e
                              | n == 1 = g e (nthS xs 0)
                              | otherwise = let (q,k) = (n `div` 2, n `mod` 2)
                                                n' = q + k
                                                xs' = contract f xs
                                            in reduceS' g e xs' n'

  scanS :: (a -> a -> a) -> a -> [a] -> ([a], a)

  fromList = id
