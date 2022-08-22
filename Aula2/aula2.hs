module Aula2 where
import Data.Char

-- ///////////////// Exercício 1 

-- a)

{-funA[2,3,5,1] = 2^2 + 3^2 + 5^2 + 1^2 + 0^2 = 39
esta função pega no primeiro elemento da lista e faz a tua potencia em 2
e chama recursivamente a função com o input da lista sem o primeiro elemento
-}

-- b)

{- funB [8,5,12] = [8,12]
esta função pega no primeiro elemento da lista e verifica se é um numero impar ou par,
caso o numero seja par, ele mantem na lista e chama recursivamente a função, mantendo
esse elemento testado dentro da lista, caso seja impar, esse elemento sera eliminado.
-}

-- c)

{- funC [1,2,3,4,5] = [5]
a função vai retirando os dois primeiros elementos da lista ou seja, o 1 e o 2, e chama
a propria função com o input de [3,4,5], em seguida ela vai remover denovo os 2 primeiros
elemento sobrando apenas [5], e como funC [x] = [x], sendo x o primeiro elemento da lista
o resultado será [5].
-}

-- d)

{-funD "otrec" = "certo"" 

    funD l = g [] l
    g acc [] = acc
    g acc (h:t) = g (h:acc) t

    recebe uma lista de char (uma string), e inverte essa string para outra string
    
    --> ??? <--

-}

-- ///////////////// Exercício 2

-- a) Done

dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = h*2 : dobros t

-- b) Done

numOcorre :: Char -> String -> Int
numOcorre c [] = 0
numOcorre letra (h:t) 
    | letra == h = 1 + numOcorre letra t
    | otherwise = 0 + numOcorre letra t

-- c) Done

positivos :: [Int] -> Bool
positivos [] = True
positivos (h:t) 
    | h > 0 = positivos t
    | otherwise = False 

-- d) Done

soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t) 
    | positivos (h:t) = (h:t)
    | h >= 0 = h : soPos t
    | otherwise = soPos t

-- e) Done

somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (h:t) 
    | h < 0 = h + somaNeg t
    | otherwise = somaNeg t

-- f) Done (Provavelmente não é da forma mais compacta, mas funciona!)

tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt (h:t)  
    | tamLista <= 3 = (h:t)
    | tamLista > 3 = tresUlt t  
        where tamLista = contaComprimento (h:t)

contaComprimento :: [a] -> Int
contaComprimento [] = 0
contaComprimento (h:t) = 1 + contaComprimento t


-- g) Done

segundos :: [(a,b)] -> [b]  -- temos que criar uma função que nos devolva o que queremos de um par
segundos [] = []
segundos (h:t) = h2 : segundos t
    where h2 = devolveSegundo h

devolveSegundo :: (a,b) -> b
devolveSegundo (x,y) = y

-- h) Done

nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros a [] = False
nosPrimeiros var (h:t)
    | var == h1 = True
    | otherwise = nosPrimeiros var t
        where h1 = devolvePrimeiro h

devolvePrimeiro :: (a,b) -> a
devolvePrimeiro (x,y) = x


-- i) Done

sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos (h:t) = adicionarTriplo h (sumTriplos t)

-- Recebe dois argumentos "h" sendo um triplo, e (sumTriplos) sendo o sengundo triplo necessario para a soma da função a baixo

adicionarTriplo :: (Num a, Num b, Num c) => (a,b,c) -> (a,b,c) -> (a,b,c)
adicionarTriplo (x,y,z) (x2,y2,z2) = (x + x2 , y + y2 , z + z2)


-- ////////////////// Exercício 3


-- a) Done

soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (h:t)
    | isDigit h = h : soDigitos t   --isDigit Char -> Bool
    | otherwise = soDigitos t


-- b) Done

minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (h:t)
    | isLower h = 1 + minusculas t  -- isLower :: Char -> Bool
    | otherwise = minusculas t 

-- c) Done

nums :: String -> [Int]
nums [] = []
nums (h:t) 
    | isDigit h = (digitToInt h) : nums t    -- digitToInt :: Char -> Int
    | otherwise = nums t


-- ///////////////////// Exercício 4

type Polinomio = [Monomio]
type Monomio = (Float,Int)

-- a) Done

conta :: Int -> Polinomio -> Int
conta n [] = 0
conta n (h:t)
    | g == n = 1 + conta n t
    | otherwise = conta n t
        where g = devolveSegundo h

-- b) Done

grau :: Polinomio -> Int
grau [] = 0
grau (h:t) = compareGrau p (grau t) -- compara o grau do polinomio atual e do seguinte
    where p = devolveSegundo h

compareGrau :: Int -> Int -> Int  -- Compara dois numeros para ver qual o maior
compareGrau num1 num2
    | num1 >= num2 = num1
    | otherwise = num2

-- c) Done

selgrau :: Int -> Polinomio -> Polinomio
selgrau grau [] = []
selgrau grau_input (h:t)
    | grau_input == grau = h : selgrau grau_input t
    | otherwise = selgrau grau_input t
        where grau = devolveSegundo h

-- d) Done

deriv :: Polinomio -> Polinomio
deriv [] = []
deriv (h:t) = derivMonomio h : deriv t
        
derivMonomio :: Monomio -> Monomio
derivMonomio (0,0) = (0,0)
derivMonomio (coef,0) = (0,0)
derivMonomio (coef,grau) = (new_coef, grau - 1)
    where new_coef = (fromIntegral (grau) * coef)

-- e) Done

calcula :: Float -> Polinomio -> Float
calcula x [] = 0
calcula x (h:t) = calculaMonomio x h + calcula x t

calculaMonomio :: Float -> Monomio -> Float
calculaMonomio x (coef,grau) = coef * (x ^ grau)

-- f) Done

simp :: Polinomio -> Polinomio
simp [] = []
simp (h:t) 
    | coefZeroMon h = h : simp t
    | otherwise = simp t

coefZeroMon :: Monomio -> Bool
coefZeroMon (coef,_)
    | coef == 0 = True
    | otherwise = False

-- g) Done

mult :: Monomio -> Polinomio -> Polinomio
mult mon [] = []
mult mon (h:t) = multMonPol mon h : mult mon t

multMonPol :: Monomio -> Monomio -> Monomio
multMonPol (0,grau) (coef2,grau2)= (0,0)
multMonPol (coef,grau) (0,grau2)= (0,0)
multMonPol (coef,grau) (coef2,grau2) = (coef * coef2 , grau + grau2)

-- h) Done

normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza (h:t) = somaMonoLista h (normaliza t)

somaMonos :: Monomio -> Monomio -> Monomio -- Sabendo que eles tem a mesma potencia !
somaMonos (x1,y1) (x2,_) = (x1+x2,y1) 

comparaMonos :: Monomio -> Monomio -> Bool -- Compara o grau dos monomios !
comparaMonos (_,y1) (_,y2) 
    | y1 == y2 = True
    | otherwise = False

somaMonoLista :: Monomio -> Polinomio -> Polinomio
somaMonoLista m [] = [m]
somaMonoLista m (h:t) 
    | comparaMonos m h = somaMonos m h : t
    | otherwise = h : somaMonoLista m t


-- i) Done

soma :: Polinomio -> Polinomio ->  Polinomio
soma [] [] = []
soma p1 [] = p1
soma [] p2 = p2
soma (h1:t1) p2 = normaliza ( somaMonoLista h1 (soma t1 p2) )


-- j) Done

produto :: Polinomio -> Polinomio -> Polinomio
produto [] [] = []
produto _ [] = []
produto [] _ = []
produto (h1:t1) p2 = multMonoLista h1 p2 ++ produto t1 p2

multMonos :: Monomio -> Monomio -> Monomio
multMonos (x1,y1) (x2,y2) = (x1 * x2 , y1 + y2) 

multMonoLista :: Monomio -> Polinomio -> Polinomio
multMonoLista m [] = []
multMonoLista m (h:t) = multMonos m h : multMonoLista m t

-- Extra Func
normProd :: Polinomio -> Polinomio
normProd [] = []
normProd p = normaliza p


-- k) 

ordena :: Polinomio -> Polinomio
ordena [] = []
ordena (h:t) = ordenaMonos h (ordena t)

-- Compara se o primeiro Monmio é menor ou não que o segundo
comparGrau :: Monomio -> Monomio -> Bool
comparGrau (_,y1) (_,y2)
    | y1 > y2 = False
    | y1 <= y2 = True

ordenaMonos :: Monomio -> Polinomio -> Polinomio
ordenaMonos m [] = [m]
ordenaMonos m (h:t)
    | comparGrau m h = m : h : t
    | comparGrau m h == False = h : t ++ [m]  -- ordenaMonos, tentar usar aqui :D
    | otherwise = h : t ++ [m]