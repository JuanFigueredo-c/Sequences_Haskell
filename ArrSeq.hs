module ArrSeq where

import Seq
import Par
import qualified Arr as A
import Arr ((!))

emptyS :: A.Arr a
emptyS = A.empty

singletonS :: a -> A.Arr a
singletonS x = A.fromList [x]

lengthS :: A.Arr a -> Int
lengthS = length

nthS :: A.Arr a -> Int -> a
nthS = (!)

tabulateS :: (Int -> a) -> Int -> A.Arr a
tabulateS = tabulate

mapS :: (a -> b) -> A.Arr a -> Arr b
mapS f (A.A xs) = A.A (fmap f xs)

filterS :: (a -> Bool) -> A.Arr a -> A.Arr a
filterS p (A.A xs) = A.A (filter p xs)

appendS :: A.Arr a -> A.Arr a -> A.Arr a
appendS = (++)

takeS :: A.Arr a -> Int -> A.Arr a
takeS xs n = A.subArray 0 n xs

dropS :: A.Arr a -> Int -> A.Arr a
dropS xs n = A.subArray n (lengthS xs) xs

showtS :: A.Arr a -> TreeView a (A.Arr a)
showtS xs | n == 0 = Seq.EMPTY
          | n == 1 = Seq.ELT (nthS xs 0)
          | otherwise = Seq.NODE (takeS xs m) (dropS xs m)
            where
              n = length xs
              m = div n 2

showlS :: A.Arr a -> ListView a (A.Arr a)
showlS xs | n == 0 = Seq.NIL
          | otherwise = Seq.CONS (nthS xs 0) (dropS xs 1) 

joinS :: A.Arr (A.Arr a) -> A.Arr a
joinS xss | n == 0 = emptyS
          | n == 1 = nthS xss 0
          | otherwise = let (l, r) =  joinS (take xss m)
                                      Par.(|||)
                                      joinS (drop xss m)
                        in appendS l r
            where
              n = length xss
              m = div n 2

reduceS :: (a -> a -> a) -> a -> A.Arr a -> a
reduceS f e xs | null xs = e
               | otherwise = f e (Seq.reduceT t)
                  where t = Seq.toTree xs

scanS :: (a -> a -> a) -> a -> A.Arr a -> (A.Arr a, a)


fromList :: [a] -> A.Arr a
fromList = A.fromList
