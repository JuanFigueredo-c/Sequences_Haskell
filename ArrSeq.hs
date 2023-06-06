module ArrSeq where

import Seq (reduceT, TreeView(..), ListView(..), Tree(..))
import Par ((|||))
import qualified Arr as A
import Arr ((!), Arr(A))

contract :: (a -> a -> a) -> A.Arr a -> A.Arr a
contract f xs = tabulateS g n'
              where
                n = lengthS xs
                k = n `div` 2
                n' = if even n then k else k + 1
                g i = if 2 * i + 1 /= n then f (nthS xs 2*i) (nthS xs (2*i + 1))
                                        else (nthS xs 2*i)

instance Seq A.Arr where
  emptyS = A.empty

  singletonS x = A.fromList [x]

  lengthS = A.length

  nthS = (!)

  tabulateS = A.tabulate

  mapS f xs = ArrSeq.tabulateS (f . ArrSeq.nthS $ xs) (lengthS xs)

  filterS p xs = joinS $ tabulateS f (lengthS xs) 
                 where f = let x = ArrSeq.nthS i
                 in if p x then singletonS x 
                           else emptyS
  
  appendS xs ys = ArrSeq.joinS $ ArrSeq.fromListS [xs, ys]

  takeS xs n = A.subArray 0 n xs

  dropS xs n = A.subArray n (ArrSeq.lengthS xs) xs

  showtS xs | n == 0 = Seq.EMPTY
            | n == 1 = Seq.ELT (ArrSeq.nthS xs 0)
            | otherwise = Seq.NODE (ArrSeq.takeS xs m) (ArrSeq.dropS xs m)
              where
                n = ArrSeq.lengthS xs
                m = div n 2

  showlS xs | n == 0 = Seq.NIL
            | otherwise = Seq.CONS (ArrSeq.nthS xs 0) (ArrSeq.dropS xs 1)
            where n = ArrSeq.lengthS xs

  joinS xss | n == 0 = ArrSeq.emptyS
            | n == 1 = ArrSeq.nthS xss 0
            | otherwise = let (l, r) =  ArrSeq.joinS (ArrSeq.takeS xss m)
                                        |||
                                        ArrSeq.joinS (ArrSeq.dropS xss m)
                          in ArrSeq.appendS l r
              where
                n = ArrSeq.lengthS xss
                m = div n 2

  reduceS f e xs | n == 0 = e
                 | n == 1 = f e $ ArrSeq.nthS xs 0
                 | n > 1 = reduceS f e $ contract f xs
                  where n = Arr.length xs
  
  scanS :: (a -> a -> a) -> a -> A.Arr a -> (A.Arr a, a)
  fromList = A.fromList

