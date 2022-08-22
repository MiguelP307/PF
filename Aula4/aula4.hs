module Aula4 where
import Data.Char 

-- ///////////////// Exercicio 4

--1) Done

digitAlpha :: String -> (String,String)
digitAlpha "" = ("","")
digitAlpha (h:t)
    | isDigit h = (h : xs , ys)
    | isAlpha h = (xs, h : ys)
    | otherwise = (xs,ys)
        where (xs,ys) = digitAlpha t

--2) Done

nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp (h:t)
    | h < 0 = (1 + as,bs,cs)
    | h == 0 = (as,1 + bs,cs)
    | otherwise = (as,bs,1 + cs) 
        where (as,bs,cs) = nzp t


--3) Done

divMode :: Integral a => a -> a -> (a,a)
divMode num divi 
    | (n < divi && n >= 0) = (1,n)
    | otherwise = (1 + as,bs)
        where (as,bs) = divMode n divi 
              n = num - divi
              

-- 4) Done

fromDigits :: [Int] -> Int
fromDigits [] = 0
fromDigits l = fDA l (length l)

fDA :: [Int] -> Int -> Int 
fDA [] 0 = 0
fDA (h:t) exp = h*10^(exp-1) + fDA t (exp-1)

--5)

maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit l = maximum [sum m | m <- inites l]

inites :: [a] -> [[a]]
inites [] = [[]]
inites list = inites (init list) ++ [list]


--6) TO DO

{- fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibA :: Int -> Int -> Int -> Int
fibA n l1 l2 =  -}


--7) TO DO
{- 
intToStr :: Integer -> String
intToStr n = iTSA n (head [ n | x <- [0..] , (fromInteger n `div` 10^x) >= 0 , (fromInteger n `div` 10^x) <= 9 ])

iTSA :: Integer -> Integer -> String
iTSA n 0 = ""
iTSA n tam = chr (res + 48) : iTSA (n - toInteger (res * mul)) (tam-1)
    where mul = 10^(tam-1)
          res = ( (fromInteger n) `div`  mul ) -}


-- ////////////////////// Exercicio 8
{- 
(a) [x | x <- [1..20], mod x 2 == 0, mod x 3 == 0] ---- "mod x 2 == 0, mod x 3 == 0" a "," funciona com um "AND" logico
(b) [x | x <- [y | y <- [1..20], mod y 2 == 0], mod x 3 == 0]
(c) [(x,y) | x <- [0..20], y <- [0..20], x+y == 30]
(d) [sum [y | y <- [1..x], odd y] | x <- [1..10]] 
-}

--a) [6,12,18]

--b) [6,12,18]

--c) [(10,20),(11,19),(12,18),(13,17),(14,16),(15,15),(16,14),(17,13),(18,12),(19,11),(20,10)]

--d) [1,1,4,4,9,9,16,16,25,25]

-- ////////////////////////Exercicio 9

--(a) [1,2,4,8,16,32,64,128,256,512,1024]
{-(b) [(1,5),(2,4),(3,3),(4,2),(5,1)]
  (c) [[1],[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5]]
  (d) [[1],[1,1],[1,1,1],[1,1,1,1],[1,1,1,1,1]]
  (e) [1,2,6,24,120,720] 
-}

--a) [2^x | x <- [0..10]]
--b) [(x,y) | x <- [1..5], y <- [1..5], x + y == 6]
--c) [ [1..x] | x <- [1..5]]  -> [[1..1],[1..2],[1..3],[1..4],[1...5]]
--d) [product [y | y <- [1..x] ] | x <- [1..6] ]














