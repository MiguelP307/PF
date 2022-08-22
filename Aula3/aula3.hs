module Aula3 where
import Aula1

-- /////////////////// Exercicio 1 TO DO
{- 
data Hora = H Int Int
    deriving (Show)

type Etapa = (Hora,Hora)

type Viagem = [Etapa]
 -}
-- a)
{- 
valHora :: Hora -> Bool
valHora (H h m)
    | h >= 0 && h <= 23 && m >= 0 && m <= 59 = True
    | otherwise = False

testHour :: Etapa -> Bool
testHour ((H h1 m1),(H h2 m2))
    | (valHora (H h1 m1) && valHora (H h2 m2)) = False
    | h1 > h2 = True
    | h1 == h2 && m1 > m2 = True
    | otherwise = False    
 -}

-- b)
{- 
valViagem :: [Viagem] -> Bool
valViagem [] = False
valViagem ((H h1 m1,H h2 m2):t)
    | valHora (H h1 m1) && valHora (H h2 m2) && not(testHour (H h1 m1,H h2 m2)) = True 
    | otherwise = False
 -}

-- ///////////////// Exercicio 2

type Poligonal = [Ponto]

{- data Ponto = Cartesiano Double Double | Polar Double Double
    deriving (Show,Eq)
 -}

--a) Done

lengthLine :: Poligonal -> Double
lengthLine ((Cartesiano _ _):[]) = 0
lengthLine (h1:h2:t)
    | posx h1 == posx h2 = abs(posy h1 - posy h2) + next 
    | posy h1 == posy h2 = abs(posx h1 - posx h2) + next
    | otherwise = sqrt((posx h1 - posx h2)^2+(posy h1 - posy h2)^2) + next
        where next = lengthLine (h2:t)

--b) Done

closeLine :: Poligonal -> Bool
closeLine [] = False
closeLine (h:t) = elem h t || closeLine t

--c) Done
{- 
data Figura = Circulo Ponto Double
            | Rectangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
                deriving (Show,Eq) -}

triangula :: Poligonal -> [Figura]
triangula [h1,h2,h3] = [Triangulo h1 h2 h3]
triangula (h1:h2:h3:t) = Triangulo h1 h2 h3 : triangula (h1:h3:t)  -- (h1:h3:p2)
triangula _ = []

--d) Done

areaTotal :: [Figura] -> Double
areaTotal [] = 0
areaTotal (h:t) = area h + areaTotal t 

--e) Done

mover :: Poligonal -> Ponto -> Poligonal
mover [] _ = []
mover l p = p : (removePnt l p)


removePnt :: Poligonal -> Ponto -> Poligonal
removePnt [] _ = []
removePnt (h:t) p
    | h /= p = h : removePnt t p
    | otherwise = removePnt t p

--f) Done

zoom :: Double ->  Poligonal -> Poligonal 
zoom _ [] = []
zoom z ((Cartesiano p1 p2):(Cartesiano p3 p4):t) = (Cartesiano p1 p2) : zoom z ((Cartesiano (p3*z) (p4*z)):t)
zoom _ [x] = [x]





-- ///////////////////////////// Exercicio 3

data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
                deriving (Show)

type Nome = String
type Agenda = [(Nome, [Contacto])]


--a) Done

acrescEmail :: Nome -> String -> Agenda -> Agenda -- Caso haja ja um nome, acrescenta so o contacto
acrescEmail _ _ [] = []
acrescEmail nome mail agend@((n,c):t2)
    | nome == n = [(nome,[Email mail])] ++ acrescEmail nome mail t2
    | otherwise = (n,c) : acrescEmail nome mail t2
{- acrescEmail nome mail agenda = agenda ++ [(nome,[Email mail])] -} --Acrescenta caso n haja


--b) Done

verEmails :: Nome -> Agenda -> Maybe [String]
verEmails _ [] = Nothing
verEmails nome [(n,c)]
    | nome == n = Just [e | Email e <- c] -- mete no E os emails que estiveres dentro da lista c
    | otherwise = Nothing
verEmails nome ((n,c):t)
    | nome == n = verEmails nome [(n,c)]  -- pega o email da head
    | otherwise = verEmails nome t        -- caso contrario ve no resto da lista

--c) Done

consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs ((Tlm n):t) = n : consTelefs t
consTelefs ((Casa n):t) = n : consTelefs t
consTelefs ((Trab n):t) = n : consTelefs t
consTelefs (_:t) = consTelefs t

-- com Case _ of _ ->

caseConstTelefs :: [Contacto] -> [Integer]
caseConstTelefs [] = []
caseConstTelefs (h:t) = case h of Tlm x -> x : caseConstTelefs t
                                  Casa x -> x : caseConstTelefs t
                                  Trab x -> x : caseConstTelefs t
                                  _ -> caseConstTelefs t
                
--d) Done

casa :: Nome -> Agenda -> Maybe Integer
casa _ [] = Nothing
casa nome [(n,c:cs)] -- [head de baixo]
    | nome == n = case c of Casa num -> Just num  -- caso C == "Casa" ent da o numero, caso n seja, vai buscar o resto da lista "c:cs" = lista de contactos
                            _ -> casa nome [(n,cs)]
    | otherwise = Nothing
casa nome ((n,c):t)
    | nome == n = casa nome [(n,c)]  -- manda para a função de cima
    | otherwise = casa nome t        -- manda para o proximo contacto para ver os numeros/emails


-- /////////////// Exercicio 4

type Dia = Int
type Mes = Int
type Ano = Int
{- type Nome = String -}
{- data Data = D Dia Mes Ano
    deriving (Show) -}

type TabDN = [(Nome,Data)]


--a) Done

procura :: Nome -> TabDN -> Maybe Data
procura _ [] = Nothing
procura name ((n,d):t)
    | name == n = Just d
    | otherwise = procura name t

--b) Done


idade :: Data -> Nome -> TabDN -> Maybe Int
idade _ _ [] = Nothing
idade date@(D d2 m2 a2) nome ((n,(D d m a)):t) 
    | nome == n && (m2 < m || (m2 == m && d2 <= d))  = Just (a2-a-1)
    | nome == n && m2 >= m = Just (a2-a)
    | otherwise = idade date nome t


--c) Done 

anterior :: Data -> Data -> Bool
anterior (D d1 m1 a1) (D d2 m2 a2)
    | a1 > a2 || (a1 == a2 && (m1 > m2 || (m1 == m2 && d1 > d2))) = False
    | otherwise = True

--d) Done

ordena :: TabDN -> TabDN
ordena [] = []
ordena (h:t) = inst [h] (ordena t)

inst :: TabDN -> TabDN -> TabDN
inst [] [] = []
inst [x] [] = [x]
inst ((n1,d1):t1) ((n2,d2):t)
    | anterior d1 d2 = [(n1,d1)] ++ ((n2,d2):t)
    | otherwise = [(n2,d2)] ++ inst [(n1,d1)] t


--e) Done

porIdade :: Data -> TabDN -> [(Nome,Int)]
porIdade _ [] = []
porIdade date l@((n,dat):t) = porIdadeAux date (reverse(ordena l))

porIdadeAux :: Data -> TabDN -> [(Nome,Int)]
porIdadeAux _ [] = []
porIdadeAux date ((n,dat):t) = (n,auxidade date n [(n,dat)]) : porIdade date t 

auxidade :: Data -> Nome -> TabDN -> Int
auxidade _ _ [] = -1
auxidade date@(D d2 m2 a2) nome ((n,(D d m a)):t) 
    | nome == n && (m2 < m || (m2 == m && d2 <= d)) = (a2-a-1)
    | nome == n && m2 >= m = (a2-a)
    | otherwise = auxidade date nome t


-- //////////////////// Exercício 5 TO DO

data Movimento = Credito Float | Debito Float
    deriving (Show)

data Data = D Int Int Int
    deriving (Show)

data Extracto = Ext Float [(Data, String, Movimento)]
    deriving (Show)


--a)
{- 
extValor :: Extracto -> Float -> [Movimento]
extValor  -}