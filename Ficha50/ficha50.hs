module Ficha50 where
import Data.Char

--1 Done

enumDP :: Int -> Int -> [Int]
enumDP i f 
    | i <= f = i : enumDP (i + 1) f
    | otherwise = []


--2 Done

enumDTP :: Int -> Int -> Int -> [Int]
enumDTP i m f
    | i <= f = i : enumDTP (i + esp) (m + esp) f
    | otherwise = []
        where esp = (m-i)


--3 Done

cat :: [a] -> [a] -> [a]
cat [] [] = []
cat l1 []= l1
cat [] l2 = l2
cat (h:t) l2 = h : cat t l2

--4 Done

givePos :: [a] -> Int -> a
givePos (h:t) 0 = h
givePos (h:t) pos = givePos t (pos-1)


--5 Done

revira :: [a] -> [a]
revira [] = []
revira (h:t) = revira t ++ [h]

--6 Done

takeF :: Int -> [a] -> [a]
takeF _ [] = []
takeF n (h:t)
    | n /= 0 = h : take (n-1) t
    | otherwise = []

--7 Done

dropF :: Int -> [a] -> [a]
dropF _ [] = []
dropF 0 l = l
dropF pos (h:t) = dropF (pos-1) t


--8 Done

zipar :: [a] -> [b] -> [(a,b)]
zipar [] _ = []
zipar _ [] = []
zipar (h1:t1) (h2:t2) = (h1,h2) : zipar t1 t2


--9 Done

replicar :: Int -> a -> [a]
replicar 0 _ = []
replicar n algo = algo : replicar (n-1) algo


--10 Done

emplastro :: a -> [a] -> [a]
emplastro _ [] = []
emplastro _ [x] = [x]                       -- Para no final n adicionar o emplastro!
emplastro n (h:t) = h : n : emplastro n t

--11 Done

agrupar :: Eq a => [a] -> [[a]]
agrupar [] = []
agrupar [x] = [[x]]
agrupar (h:t)
    | elem h (head r) = (h : (head r)) : tail r
    | otherwise = [h] : r
    where r = agrupar t

--12 Done

--concat
comGato :: [[a]] -> [a]
comGato [] = []
comGato (h:t) = h ++ comGato t 

{- Ex: >comGato [[1,1],[2],[3,3,3]]
[1,1] ++ ([2] ++ ([3,3,3] ++ ([]))) = [1,1,2,3,3,3] -} 

--13 Done

inites :: [a] -> [[a]]
inites [] = [[]]
inites list = inites (init list) ++ [list]

-- Ex: [1,2,3,4]
-- inites [1,2,3,4] = ( [[]] ++ ( [[1]] ++ ( [[1,2]] ++ ( [[1,2,3]] ++ ( [[1,2,3,4]] )))))

--14 Done

tailes :: [a] -> [[a]]
tailes [] = [[]]
tailes list = list : tailes (tail list)

--15 Done

heades :: [[a]] -> [a]
heades [] = []
heades ([]:t) = heades t
heades ((hh:ht):t) = hh : heades t

--16 Done

totale :: [[a]] -> Int
totale [] = 0
totale (h:t) = (length h) + totale t

--17 Done

fune :: [(a,b,c)] -> [(a,c)]
fune [] = []
fune ((a,_,c):t) = (a,c) : fune t

--18 Done

colah :: [(String,b,c)] -> String
colah [] = ""
colah ((str,_,_):t) = str ++ colah t

--19 Done

idadeh :: Int -> Int -> [(String,Int)] -> [String]
idadeh _ _ [] = []
idadeh ano idade ((nome,anoNasc):t)
    | idadeF >= idade = nome : (idadeh ano idade t)
    | otherwise = idadeh ano idade t
        where idadeF = (ano - anoNasc)

--20 Done

poderEnumFrom :: Int -> Int -> [Int]
poderEnumFrom _ 0 = [1]
poderEnumFrom n m 
    | m /= 0  = poderEnumFrom n (m-1) ++ [(n^m)]
    | otherwise = []


-- outra resolução
pEF :: Int-> Int-> [Int]
pEF _ 1 = [1]
pEF n m = pEF n (m-1) ++ [n^(m-1)]

--21 Done

isPrime :: Int -> Bool
isPrime 2 = True
isPrime n
    | n > 2 = primeCheck n 2
    | otherwise = False

primeCheck :: Int -> Int -> Bool
primeCheck n m
    | m * m > n = True               -- equivalente a: m > √n (assim trabalhamos apenas com valores inteiros)
    | mod n m == 0 = False
    | otherwise = primeCheck n (m + 1)

-- `primeCheck` percorre os números de 2 a √n e verifica se algum destes divide n com resto 0.
-- Caso tal não se verifique para nenhum destes valores, n é primo.

--22 Done

isPrefixOfe :: Eq a => [a] -> [a] -> Bool
isPrefixOfe [] _ = True
isPrefixOfe _ [] = False
isPrefixOfe (h1:t1) (h2:t2) 
    | h1 == h2 = isPrefixOfe t1 t2
    | otherwise = False


--23 Done

isSuffixOfe :: Eq a => [a]-> [a] -> Bool
isSuffixOfe [] _ = True
isSuffixOfe _ [] = False
isSuffixOfe list (h2:t2) = list == (h2:t2) || isSuffixOfe list t2

isSuffixOfee :: Eq a => [a] -> [a] -> Bool
isSuffixOfee [] _ = True
isSuffixOfee _ [] = False
isSuffixOfee l1 l2 
    | last l1 == last l2 = isSuffixOfee (init l1) (init l2)
    | otherwise = False

--24 Done

isSubsequenceOfe :: Eq a => [a] -> [a] -> Bool
isSubsequenceOfe [] _ = True
isSubsequenceOfe _ [] = False
isSubsequenceOfe (h1:t1) (h2:t2)
    | h1 == h2 = isSubsequenceOfe t1 t2
    | otherwise = isSubsequenceOfe (h1:t1) t2

--25 Done 

elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices _ [] = []
elemIndices n l = elemIndicesPos n l 0

elemIndicesPos :: Eq a => a -> [a] -> Int -> [Int]
elemIndicesPos _ [] _ = []
elemIndicesPos n (h:t) pos 
    | n == h = pos : elemIndicesPos n t (pos+1)
    | otherwise = elemIndicesPos n t (pos+1)

--26 Done

noob :: Eq a => [a] -> [a]
noob [] = []
noob (h:t)
    | elem h t = noob t                 -- Verifica se um determinado elemento "h" esta presente na lista "t"
    | otherwise = noob t ++ [h]
--27 Done

deleteh :: Eq a => a -> [a]-> [a]
deleteh _ [] = []
deleteh n (h:t) 
    | n == h = t
    | otherwise = h : deleteh n t

--28 Done

removeSL :: Eq a => [a] -> [a] -> [a]
removeSL [] [] = []
removeSL [] _ = []
removeSL l [] = l
removeSL (h1:t1) (h2:t2)
    | h1 == h2 = removeSL t1 t2
    | otherwise = h1 : removeSL t1 (h2:t2)

--29 Done

onion :: Eq a => [a] -> [a] -> [a]
onion [] [] = []
onion [] l = l
onion l [] = l
onion l1 (h:t)
    | elem h l1 = onion l1 t
    | otherwise = onion l1 t ++ [h]


--30 Done

intersect :: Eq a => [a] -> [a] -> [a]
intersect [] [] = []
intersect l [] = l
intersect [] _ = []
intersect (h1:t1) list2
    | elem h1 list2 = h1 : intersect t1 list2
    | otherwise = intersect t1 list2

--31 Done

inserte :: Ord a => a -> [a] -> [a]
inserte n [] = [n]
inserte n (h:t)
    | n <= h = n : (h:t)
    | otherwise = h : inserte n t


--32 Done

unwordes :: [String] -> String
unwordes [] = ""
unwordes (h:t) = h ++ " " ++ unwordes t


--33 Done

unlaines :: [String] -> String
unlaines [] = ""
unlaines (h:t) = h ++ "\n" ++ unlaines t


--34 To Do
pMaior :: Ord a => [a] -> Int
pMaior (h:t) = pMAux (h,0,0) (h:t)

pMAux :: Ord a => (a,Int,Int) -> [a] -> Int
pMAux (m,pm,pa) [] = pm
pMAux (m,pm,pa) (h:t)
    | h > m = pMAux (m,pa,pa+1) t
    | otherwise = pMAux (m,pm,pa+1) t

--35 Done

lookeup :: Eq a => a -> [(a,b)] -> Maybe b
lookeup _ [] = Nothing
lookeup n ((a,b):t)
    | n == a = Just b
    |otherwise = lookeup n t


--36 Done

preCrescente :: Ord a => [a] -> [a]
preCrescente [] = []
{- preCrescente [x] = [x] -}
preCrescente (h1:h2:t)
    | h1 <= h2 = h1 : preCrescente (h2:t)
    | otherwise = [h1]


--37 Done

iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:t) = inserte h (iSort t)  

{- 
Ex: iSort([2,3,1,4])
    insert 2 ( insert 3 (inserte 1 (inserte 4  [])))
    insert 2 ( insert 3 (inserte 1 [4]))
    insert 2 ( insert 3 [1,4]))
    insert 2 [1,3,4]
    [1,2,3,4]
 -}

--38 Done

menor :: String -> String -> Bool
menor "" "" = False
menor "" _ = True
menor _ "" = False
menor (h1:t1) (h2:t2)
    | l1 < l2 = True
    | l1 == l2 = menor t1 t2
    | otherwise = False
        where l1 = ord (toUpper h1)
              l2 = ord (toUpper h2)

--39 Done

elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet _ [] = False
elemMSet n ((a,_):t)
    | n == a = True
    | otherwise = elemMSet n t

--40 Done

converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((a,num):t)
    | num /= 0 = a : converteMSet ((a,num - 1):t)
    | otherwise = converteMSet t

--41 Done

insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet n [] = [(n,1)]
insereMSet n ((a,num):t)
    | n == a = (a,num+1):t
    | otherwise = (a,num) : insereMSet n t


--42 Done

removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet _ [] = []
removeMSet n ((a,num):t)
    | n == a = removeMSet n t
    | otherwise = (a,num) : removeMSet n t

--43 Done  (Embora fique aocontrario)
  
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (h:t) = insereMSet h (constroiMSet t)

--44 Done

partitionEitheres :: [Either a b] -> ([a],[b]) 
partitionEitheres [] = ([],[])
partitionEitheres ((Left x):t) = (x : as , bs)
    where (as,bs) = partitionEitheres t
partitionEitheres ((Right y):t) = (as, y : bs)
    where (as,bs) = partitionEitheres t

-- Solução com "case of"
pE :: [Either a b] -> ([a],[b])
pE [] = ([],[])
pE (h:t) = case h of Left x -> (x:xs,ys)
                     Right y -> (xs,y:ys)
    where (xs,ys) = pE t



--45 Done

gatoMaybes :: [Maybe a] -> [a]
gatoMaybes [] = []
gatoMaybes ((Just x):t) = x : gatoMaybes t
gatoMaybes ((Nothing):t) = gatoMaybes t


--46 Done

data Movimento = Norte | Sul | Este | Oeste
    deriving (Show)

{- Norte = 1
Sul = (-1)
Este = 1
Oeste = (-1) -}

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x1,y1) (x2,y2)
    | x1 < x2 = Este : caminho (x1+1,y1) (x2,y2)
    | x1 > x1 = Oeste : caminho (x1-1,y1) (x2,y2)
    | y1 < y2 = Norte : caminho (x1,y1+1) (x2,y2)
    | y1 > y2 = Sul : caminho (x1,y1-1) (x2,y2)
    | otherwise = []


--47 Done

-- verificamos se a posição final é igual à inicial
-- se não for, removemos o último movimento (init)
-- e voltamos a verificar

hasLoops :: (Int,Int) -> [Movimento] -> Bool
hasLoops _ [] = False
hasLoops cord list = cord == cordFinais cord list || hasLoops cord (init list)
    
cordFinais :: (Int,Int) -> [Movimento] -> (Int,Int)
cordFinais p [] = p
cordFinais (x,y) (Norte:t) = cordFinais (x,y+1) t
cordFinais (x,y) (Sul:t) = cordFinais (x,y-1) t
cordFinais (x,y) (Este:t) = cordFinais (x+1,y) t
cordFinais (x,y) (Oeste:t) = cordFinais (x-1,y) t

--48 Done

-- Tendo em concideração que sao poligonos!

type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto

contaQuadrados :: [Rectangulo] -> Int
contaQuadrados [] = 0
contaQuadrados (h:t)
    | eQuadrado h = 1 + contaQuadrados t
    | otherwise = contaQuadrados t

eQuadrado :: Rectangulo -> Bool            
eQuadrado (Rect (x1,y1) (x2,y2))
    | abs(x1-x2) == abs(y1-y2) = True
    | otherwise = False

-- outra forma

eQ :: [Rectangulo] -> Int
eQ [] = 0
eQ ((Rect (x1,y1) (x2,y2)):t) 
    | abs(x2-x1) == abs (y2-y1) = 1 + eQ t
    | otherwise = eQ t


--49 Done

{- type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto -}

-- Tendo em concideração que sao poligonos!

areaTotal :: [Rectangulo] -> Float
areaTotal [] = 0
areaTotal (h:t) = areaR h + areaTotal t

areaR :: Rectangulo -> Float
areaR (Rect (x1,y1) (x2,y2)) = abs(x1-x2) * abs(y1-y2)


--50 Done

data Equipamento = Bom | Razoavel | Avariado
    deriving Show

naoReparar :: [Equipamento] -> Int
naoReparar [] = 0
naoReparar ((Avariado):t) = 1 + naoReparar t
naoReparar (h:t) = 0 + naoReparar t


-- Com case of

nR :: [Equipamento] -> Int
nR [] = 0
nR (h:t) = case h of Avariado -> 0 + nR t
                     _ -> 1 + nR t

