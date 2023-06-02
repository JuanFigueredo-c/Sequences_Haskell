module ListSeq where

import Par
import Seq

emptyS :: [a]
emptyS = []

singletonS :: a -> [a]
singletonS x = [x]

lengthS :: [a] -> Int
lengthS = length

nthS :: [a] -> Int -> a
nthS xs i = xs ! i

tabulateS :: (Int -> a) -> Int -> [a]
tabulateS f n = map f [0..(n-1)]

mapS :: (a -> b) -> [a] -> [b]
mapS = map

filterS :: (a -> Bool) -> [a] -> [a] 
filterS = filter

appendS :: [a] -> [a] -> [a]
appendS = (++)

takeS :: [a] -> Int -> [a]
takeS = take

dropS :: [a] -> Int -> [a]
dropS = drop

showtS   :: [a] -> TreeView a ([a])
showtS [] = EMPTY
showtS [x] = ELT x
showtS xs = let n = (div (length xs) 2)
            in NODE (take xs n) (drop xs n)

showlS :: [a] -> ListView a ([a])
showlS [] = NIL
showlS (x:xs) = CONS x xs

joinS :: s ([a]) -> [a]
joinS = foldr (++) []

reduceS :: (a -> a -> a) -> a -> [a] -> a
reduceS g e [] = e
reduceS g e xs = g e (reduceT g t)
                  where t = toTree xs

scanS :: (a -> a -> a) -> a -> [a] -> ([a], a)

fromList :: [a] -> [a]
fromList = id
