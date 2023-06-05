{- Implementación del TAD secuencia -}

module Seq (reduceT, TreeView(..), ListView(..), Tree(..)) where

import Par ((|||))

class Seq s where
  emptyS     :: s a
  singletonS :: a -> s a
  lengthS    :: s a -> Int 
  nthS       :: s a -> Int -> a 
  tabulateS  :: (Int -> a) -> Int -> s a
  mapS       :: (a -> b) -> s a -> s b 
  filterS    :: (a -> Bool) -> s a -> s a 
  appendS    :: s a -> s a -> s a
  takeS      :: s a -> Int -> s a
  dropS      :: s a -> Int -> s a
  showtS     :: s a -> TreeView a (s a)
  showlS     :: s a -> ListView a (s a)
  joinS      :: s (s a) -> s a
  reduceS    :: (a -> a -> a) -> a -> s a -> a
  scanS      :: (a -> a -> a) -> a -> s a -> (s a, a)
  fromList   :: [a] -> s a

data TreeView a t = EMPTY | ELT a | NODE t t
data ListView a t = NIL | CONS a t

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving Show -- TODO: Sacar esto
reduceT :: (a -> a -> a) -> Tree a -> a
reduceT g (Leaf x) = x
reduceT g (Node l r) = let (l',r') = (reduceT g l)
                                      ||| (reduceT g r)
                       in g l' r'
