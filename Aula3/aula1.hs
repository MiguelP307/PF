{-/////////////////////////////// Notas //////////////////////////////-}

{- 
Ifs in Haskell  if ... then .... else ... , é preciso ter sempre os três, se não vai dar erro

Mod -> Faz o resto de uma divisão
Odd [int] -> verifica se um int é impar
Even [int] -> Verificia se um numero e par
Tail [lista] -> Remove o primeiro elemento da lista e da output do resto
Lenght [l] -> Retorna o tamanho de uma lista
-}

{-/////////////////////////////// EXERCICIO 1 //////////////////////////////-}

module Aula1 where
import Data.Char

perimetro :: Double -> Double -- Tipo do do input e do output
perimetro r = 2 * r * pi 

dist :: Double -> Double -> Double -> Double -> Double
dist x1 y1 x2 y2 = sqrt((x2-x1)^2 + (y2-y1)^2)

primUlt :: [a]  -> (a,a)                                -- [a] "pega" o tipo do input e "coloca na variavel" [a]
primUlt l = (head l , last l) 


multiplo :: Int -> Int -> Bool
multiplo m n = if mod n m == 0 then True else False     


truncaImpar :: Num a => [a] -> [a]                      
truncaImpar l = if odd (length l) then tail l else l      


maior2 :: Int -> Int -> Int                             
maior2  x y = if x <= y then y else x                   

maior3 :: Int -> Int -> Int -> Int                      
maior3 x y z = maior2 z (maior2 x y)                    -- (maior2 x y) é tipo um bloco que representa uma variavel, q vai servir como segundo input da primeira fun "maior2"


{-/////////////////////////////// EXERCICIO 2 //////////////////////////////-}

nRaizes :: Ord a => Num a => a -> a -> a -> Int
nRaizes a b c
    | delta == 0 = 1
    | delta > 0 = 2
    | delta < 0 = 0
        where delta = (b^2 - (4*a*c))


raizes :: (Ord a, Floating a) => a -> a -> a -> [a]
raizes a b c 
    | nRaizes a b c == 1 = [(-b) / 2 * a]
    | nRaizes a b c == 2 = [((- b) + sqrt(delta) ) / (2 * a) ,((- b) - sqrt(delta) ) / (2 * a) ]
    | otherwise = []
        where delta = (b^2 - (4*a*c))


{-/////////////////////////////// EXERCICIO 3 //////////////////////////////-}

-- a)
type Hora = (Int,Int)

valHour :: Hora -> Bool
valHour (h,m)
    | h >= 0 && h <= 23 && m >= 0 && m <= 59 = True
    | otherwise = False

-- Testa se a primeira hora é depois ou não da segunda

-- b)

testHour :: [Hora] -> Bool
testHour [(h1,m1),(h2,m2)] = if (valHour (h1,m1) && valHour (h2,m2)) == False
                                then error "Invalid Hour input"
                                else if h1 > h2
                                then True
                                else if h1 == h2 && m1 > m2
                                    then True
                                    else False

-- c)

convertHour :: Hora -> Int
convertHour (0,0) = 0
convertHour (a,b) 
    | valHour (a,b) = a * 60 + b

-- d)

convertMinutes :: Int -> Hora
convertMinutes 0 = (0,0)
convertMinutes mins 
    | valHour hour = hour
    | otherwise = error "Invalid Hour"
        where hour = (mins `div` 60 ,mod mins 60)

-- e)

difHours :: (Hora,Hora) -> Int 
difHours ((0,0),(0,0)) = 0
difHours (h1,h2) = abs (convertHour h1 - convertHour h2)

-- f)

addMins :: Hora -> Int -> Hora
addMins h 0 = h
addMins h m = convertMinutes((convertHour h) + m) 

{-/////////////////////////////// EXERCICIO 4 //////////////////////////////-}

data Hora1 = H Int Int 
    deriving (Show,Eq)

valHourE4 :: Hora1 -> Bool
valHourE4 (H h m)
    | h >= 0 && h <= 23 && m >= 0 && m <= 59 = True
    | otherwise = False
 
{- Basicamente a merda do exercicio é so substituir esta menha, e dps é escrever "valHourE4 (H <hora> <mins>)" -}

{-/////////////////////////////// EXERCICIO 5 //////////////////////////////-}

data Semaforo = Verde | Amarelo | Vermelho 
    deriving (Show,Eq)

-- a)

next :: Semaforo -> Semaforo
next Verde = Amarelo
next Amarelo = Vermelho
next Vermelho = Verde

-- b)

stop :: Semaforo -> Bool
stop Vermelho = True
stop x = False

-- c)

safe :: Semaforo -> Semaforo -> Bool
safe s1 s2
    | s1 == Vermelho || s2 == Vermelho = True
    | otherwise = False

{-/////////////////////////////// EXERCICIO 6 //////////////////////////////-}

data Ponto = Cartesiano Double Double | Polar Double Double
    deriving (Show,Eq)

-- a)

posx :: Ponto -> Double
posx (Cartesiano x y) = abs(x)

-- b)

posy :: Ponto -> Double
posy (Cartesiano x y) = abs(y)

-- c)

raio :: Ponto -> Double
raio (Cartesiano 0 0) = 0
raio (Cartesiano x y) = sqrt(( x^2 ) + ( y^2 ))

-- d)

angulo :: Ponto -> Double
angulo (Cartesiano x y) = sin( posy (Cartesiano x y) / raio (Cartesiano x y) )

-- e)

dist2 :: Ponto -> Ponto -> Double
dist2 (Cartesiano x1 y1) (Cartesiano x2 y2) = sqrt(( x2 - x1 )^2 + ( y2 - y1 )^2)


{-/////////////////////////////// EXERCICIO 7 //////////////////////////////-}

data Figura = Circulo Ponto Double
            | Rectangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
                deriving (Show,Eq)

-- a)

posxWA :: Ponto -> Double
posxWA (Cartesiano x y) = x

posyWA :: Ponto -> Double
posyWA (Cartesiano x y) = y


poligono :: Figura -> Bool
poligono (Circulo p r) = False
poligono (Rectangulo v1 v2) = (posxWA v1 /= posxWA v2 && posyWA v1 /= posyWA v2)
poligono (Triangulo v1 v2 v3) = not ((posxWA v1 == posxWA v2) && (posxWA v2 == posxWA v3) || (posyWA v1 == posyWA v2) && (posyWA v2 == posyWA v3))

{- Testes example:
    poligono (Rectangulo (Cartesiano 2 2) (Cartesiano 5 4))
    poligono (Circulo (Cartesiano 1 1) 2)
    poligono (Triangulo (Cartesiano 0 0) (Cartesiano 1 1) (Cartesiano 0 1)) -}

-- b)

vertices :: Figura -> [Ponto]
vertices (Circulo p r) = []
vertices (Rectangulo v1 v2) 
    | poligono (Rectangulo v1 v2) = [v1, v2, Cartesiano (posxWA v1) (posyWA v2), Cartesiano (posxWA v2) (posyWA v1)]
    | otherwise = []
vertices triang@(Triangulo v1 v2 v3)
    | poligono triang = [v1,v2,v3]
    | otherwise = []

-- c)

area :: Figura -> Double
area (Triangulo p1 p2 p3) =
    let a = dist2 p1 p2
        b = dist2 p2 p3
        c = dist2 p3 p1
        s = (a+b+c) / 2 -- semi-perimetro
    in sqrt (s*(s-a)*(s-b)*(s-c)) -- formula de Heron
area circ@(Circulo p r) = pi * (r^2)
area rect@(Rectangulo v1 v2)
    | poligono rect = abs(posyWA v2 - posyWA v1) * abs(posxWA v1 - posxWA v2)
    | otherwise = 0

-- d)

perimetro2 :: Figura -> Double
perimetro2 ret@(Rectangulo v1 v2)
    | poligono ret = abs(posyWA v2 - posyWA v1) * 2 + abs(posxWA v1 - posxWA v2) * 2
    | otherwise = 0
perimetro2 (Circulo p r) = 2 * pi * r
perimetro2 triang@(Triangulo v1 v2 v3)
    | poligono triang = dist2 v1 v2 + dist2 v2 v3 + dist2 v3 v1
    | otherwise = 0

{-/////////////////////////////// EXERCICIO 8 //////////////////////////////-}

-- a)

isBaixo :: Char -> Bool
isBaixo char 
    | code >= 97 && code <= 122 = True
    | otherwise = False
        where code = ord char

-- b)

isDigito :: Char -> Bool
isDigito dig
    | code >= 48 && code <= 57 = True
    | otherwise = False
        where code = ord dig

-- c)

isAlphas :: Char -> Bool
isAlphas char
    | code >= 97 && code <= 122 || code >= 65 && code <= 90 = True 
    | otherwise = False
        where code = ord char

-- d)

toGraunde  :: Char -> Char
toGraunde char 
    | isBaixo char = chr ((ord char) - 32)
    | otherwise = '-'

-- e)

intToDigito :: Int -> Char
intToDigito num 
    | isDigito n = n
    | otherwise = '-'
        where n = chr(num + 48)

-- f)

digitoToInt :: Char -> Int
digitoToInt num
    | isDigito num = (ord num - 48)
