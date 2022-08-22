module Aula5 where

-- /////////////////////// Exercicio 1

--a) Done

anie :: (a -> Bool) -> [a] -> Bool
anie fun [] = False
anie fun (h:t) = fun h || anie fun t

--b) Done

zipWithe :: (a->b->c) -> [a] -> [b] -> [c]
zipWithe _ [] [] = []
zipWithe _ [] _ = []
zipWithe _ _ [] = []
zipWithe fun (h1:t1) (h2:t2) = fun h1 h2 : zipWithe fun t1 t2 

--c) Done

takeWhileh :: (a -> Bool) -> [a] -> [a]
takeWhileh _ [] = []
takeWhileh fun (h:t) 
    | fun h = h : takeWhileh fun t
    | otherwise = []

--d) Done

dropWhileh :: (a -> Bool) -> [a] -> [a]
dropWhileh _ [] = []
dropWhileh fun (h:t)
    | fun h = dropWhileh fun t
    | otherwise = h:t

--e) Done

spane :: (a -> Bool) -> [a] -> ([a],[a]) 
spane _ [] = ([],[])
spane fun (h:t)
    | fun h = (h : a1,a2)
    | otherwise = ([],h:t)
        where (a1,a2) = spane fun t


--f)

deleteBye :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBye _ _ [] = []
deleteBye fun x (h:t)
    | fun x h = t
    | otherwise = h : deleteBye fun x t

--g) Done

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn _ [] = []
sortOn fun (h:t) = sOA fun h (sortOn fun t)

sOA :: Ord b => (a -> b) -> a -> [a] -> [a]
sOA f x [] = [x]
sOA f x (h:t)
    | f x <= f h = x : h : t
    | otherwise = h : (sOA f x t)


-- //////////////////////////// Exercicio 2

type Polinomio = [Monomio]
type Monomio = (Float,Int)

--a) 

selecionaGrau :: Int -> Polinomio -> Polinomio
selecionaGrau n p = filtra (\x -> snd x == n) p

filtra :: (Monomio -> Bool) -> Polinomio -> Polinomio
filtra _ [] = []
filtra fun (h:t)
    | fun h = h : filtra fun t
    | otherwise = filtra fun t

--b) Done

conta :: Int -> Polinomio -> Int 
conta n p = count (\x -> snd x == n) p 

count :: (Monomio -> Bool) -> Polinomio -> Int
count _ [] = 0
count fun (h:t)
    | fun h = 1 + count fun t
    | otherwise = count fun t


--c)
{- 
grau :: Polinomio -> Int 
grau p = -} 


-- Fazer algo do genero do pMaior da ficha 50 !!|