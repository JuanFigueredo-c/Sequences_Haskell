module ListSeq where

import Par
import Seq

-- Funcion auxiliar para reduceS y scanS
-- Contrae una secuencia aplicando el operador recibido de par en par sobre los 
-- elementos de la secuencia. Si la cantidad de elementos es impar, deja el ultimo
-- sin operar.
contract :: (a -> a -> a) -> [a] -> [a]
contract _ [] = [] -- Contraccion de la secuencia vacia
contract _ [x] = [x] -- Contraccion de la secuencia de un solo elemento
contract f (x:y:s) = let (z, zs) = (f x y) ||| contract f s in (z:zs) -- Paralelizacion implementada para la optimizacion 
                                                                      -- de la profundidad de la funcion
-- Funcion auxiliar para scanS
-- Expande una secuencia que fue comprimida y reducida previamente
-- Los elementos que se encuentran en posicion par se mantienen como eran en 
-- la secuencia recibida. 
-- Los elementos de posiciones impares i son los resultados de operar el elemento anterior
-- con el elemento de la posicion 2*i - 1 de la secuencia original recibida.
expand :: (a -> a -> a) -> [a] -> [a] -> [a]
expand f ps s = expand' f ps s
              where

                expand' f rs [] = rs -- Expansión de la secuencia vacia 
                expand' f rs [x] = rs -- Expansión de la secuencia de un solo elemento
                expand' f (r:rs) (s1:s2:ss) = let (r', rs') = (f r s1) |||    -- Se computa el caso de la posicion par e impar
                                                              expand' f rs ss -- dentro del mismo caso. Paralelizacion agregada
                                              in r:r':rs'                     -- para optimizacion de la profundidad

instance Seq [] where
  emptyS = []  -- Implementación de la secuencia vacía con listas

  singletonS x = [x] -- Implementación de la secuencia de un solo elemento con listas

  lengthS = length -- Implementación de la funcion enghtS para secuencias con listas

  nthS xs i = xs !! i -- Implementación de la indexacion en secuencias con listas

  tabulateS f n = mapS f [0..(n-1)]  -- Implementación de la funcion tabulateS con listas


  -- Implementación de la funcion mapS con listas
  mapS f [] = []
  mapS f (x:xs) = let (y,ys) = f x ||| mapS f xs in y:ys -- Implementación de paralelizacion necesaria para
                                                         -- una optimizacion de la profundidad de la funcion.

  filterS p [] = [] -- Implementación de la funcion filterS con listas
  filterS p (x:xs) = let (px, xs') = p x ||| filterS p xs
                     in if px then x:xs' else xs'

  appendS = (++) -- Implementación de la concatenacion de secuencias con listas

  takeS s n = take n s -- Implementación de la función takeS con listas

  dropS s n = drop n s -- Implementación de la función dropS con listas

  showtS [] = EMPTY
  showtS [x] = ELT x
  showtS xs = let n = (div (length xs) 2) in NODE (take n xs) (drop n xs)

  showlS [] = NIL
  showlS (x:xs) = CONS x xs

  joinS = foldr (++) [] -- Implementación de joinS con listas

  -- Implementación de la función reduceS con listas
  reduceS f e []  = e -- reducción de una secuencia vacia, devuelve el elemento recibido 'e'
  reduceS f e [x] = f e x -- reducción de una secuencia de un solo elemento, opera el elemento 'e' con el de la secuencia
  reduceS f e xs  = let xs' = contract f xs in reduceS f e xs' -- reducción para una secuencia de múltiples elementos, 
                                                               -- se aplica la funcion reduceS sobre una contracción de la secuencia
                                                               -- original con respecto al operador recibido.
  -- Implementación de la función scanS
  scanS f e []  = (emptyS, e) -- scanS de la secuencia vacia
  scanS f e [x] = (singletonS e, f e x) --scanS de la secuencia de un solo elemento
  scanS f e xs  = let (s',r) = scanS f e $ contract f xs in (expand f s' xs, r) -- scanS de la secuencia de múltiples elementos
                                                                                -- Se contrae la secuencia antes de ser operada y luego
                                                                                -- se expande para conseguir el resultado final.

  fromList = id

