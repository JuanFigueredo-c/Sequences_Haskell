module ArrSeq where

import Seq (reduceT, TreeView(..), ListView(..), Tree(..))
import Par ((|||))
import qualified Arr as A

-- FIXME: Dont include vector
import qualified Data.Vector as V
import GHC.Float (int2Float)

import Arr ((!), Arr(A))

emptyS :: A.Arr a
emptyS = A.empty

singletonS :: a -> A.Arr a
singletonS x = A.fromList [x]

lengthS :: A.Arr a -> Int
lengthS = A.length

nthS :: A.Arr a -> Int -> a
nthS = (!)

tabulateS :: (Int -> a) -> Int -> A.Arr a
tabulateS = A.tabulate

mapS :: (a -> b) -> A.Arr a -> Arr b
mapS f xs = A.A (fmap f (A.getVector xs))
{-mapS f xs | n == 0 = emptyS
          | n == 1 = let x = nthS xs 0 in singletonS $ f x
          | otherwise = let (l, r)  = takeS xs m ||| dropS xs m
                            (l',r') = mapS f l ||| mapS f r
                        in appendS 
-}

filterS :: (a -> Bool) -> A.Arr a -> A.Arr a
filterS p xs = A.A (V.filter p (A.getVector xs))

appendS :: A.Arr a -> A.Arr a -> A.Arr a
appendS xs ys = A.A ((A.getVector xs) V.++ (A.getVector ys))

takeS :: A.Arr a -> Int -> A.Arr a
takeS xs n = A.subArray 0 n xs

dropS :: A.Arr a -> Int -> A.Arr a
dropS xs n = A.subArray n (ArrSeq.lengthS xs) xs

showtS :: A.Arr a -> TreeView a (A.Arr a)
showtS xs | n == 0 = Seq.EMPTY
          | n == 1 = Seq.ELT (ArrSeq.nthS xs 0)
          | otherwise = Seq.NODE (ArrSeq.takeS xs m) (ArrSeq.dropS xs m)
            where
              n = ArrSeq.lengthS xs
              m = div n 2

showlS :: A.Arr a -> ListView a (A.Arr a)
showlS xs | n == 0 = Seq.NIL
          | otherwise = Seq.CONS (ArrSeq.nthS xs 0) (ArrSeq.dropS xs 1)
          where n = ArrSeq.lengthS xs

joinS :: A.Arr (A.Arr a) -> A.Arr a
joinS xss | n == 0 = ArrSeq.emptyS
          | n == 1 = ArrSeq.nthS xss 0
          | otherwise = let (l, r) =  ArrSeq.joinS (ArrSeq.takeS xss m)
                                      |||
                                      ArrSeq.joinS (ArrSeq.dropS xss m)
                        in ArrSeq.appendS l r
            where
              n = ArrSeq.lengthS xss
              m = div n 2

reduceS :: (a -> a -> a) -> a -> A.Arr a -> a
reduceS f e xs | (ArrSeq.lengthS xs) == 0 = e
               | otherwise = f e (Seq.reduceT f t)
                  where t = ArrSeq.toTree xs

--scanS :: (a -> a -> a) -> a -> A.Arr a -> (A.Arr a, a)

fromList :: [a] -> A.Arr a
fromList = A.fromList

toTree :: A.Arr a -> Tree a
toTree s = case ArrSeq.lengthS s of
  1 -> (Leaf (ArrSeq.nthS s 0))
  n -> let (l,r) = toTree (ArrSeq.takeS s pp) ||| toTree (ArrSeq.dropS s pp)
       in Node l r
    where
      pp = 2 ^ (ilg (n - 1))
      ilg = floor . (logBase 2) . int2Float
