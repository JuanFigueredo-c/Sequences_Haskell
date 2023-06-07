module ArrSeq where

import Seq
import Par
import qualified Arr as A

contract :: (a -> a -> a) -> A.Arr a -> A.Arr a
contract f xs = tabulateS g n'
              where
                n = lengthS xs
                k = n `div` 2
                n' = if even n then k else k + 1
                g i = if 2 * i + 1 /= n then f (xs A.! (2*i)) (xs A.! (2*i + 1))
                                        else (xs A.! (2*i))

expand :: (a -> a -> a) -> A.Arr a -> A.Arr a -> A.Arr a
expand f ps s = tabulateS g (lengthS s)
                where
                  g i | even i = ps A.! (div i 2)
                      | otherwise = let (x, y) = ps A.! (div i 2) |||
                                                 s  A.! (i - 1)
                                    in f x y

instance Seq A.Arr where
  emptyS = A.empty

  singletonS x = Seq.fromList [x]

  lengthS = A.length

  nthS = (A.!)

  tabulateS = A.tabulate

  mapS f xs = tabulateS (f . (nthS xs)) (lengthS xs)

  filterS p xs = joinS $ tabulateS f (lengthS xs) 
                where f i = let x = nthS xs i
                            in if p x then singletonS x 
                                      else emptyS
  
  appendS xs ys = joinS $ fromList [xs, ys]

  takeS xs n = A.subArray 0 n xs

  dropS xs n = A.subArray n ((lengthS xs) - n) xs

  showtS xs | n == 0 = EMPTY
            | n == 1 = ELT (nthS xs 0)
            | otherwise = NODE (takeS xs m) (dropS xs m)
              where
                n = lengthS xs
                m = div n 2

  showlS xs | n == 0 = NIL
            | otherwise = CONS (nthS xs 0) (dropS xs 1)
            where n = lengthS xs

  joinS xss | n == 0 = emptyS
            | n == 1 = nthS xss 0
            | otherwise = let (l, r) =  joinS (takeS xss m)
                                        |||
                                        joinS (dropS xss m)
                          in appendS l r
              where
                n = lengthS xss
                m = div n 2

  reduceS f e xs | n == 0 = e
                 | n == 1 = f e $ nthS xs 0
                 | n > 1 = reduceS f e $ contract f xs
                  where n = lengthS xs
  
  scanS f e xs | n == 0 = (emptyS, e)
               | n == 1 = let x = nthS xs 0 in (singletonS e, f e x)
               | otherwise = let (s', t) = scanS f e $ contract f xs in (expand f s' xs, t)
              where n = lengthS xs

  fromList = A.fromList
