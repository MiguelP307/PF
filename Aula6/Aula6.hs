module Aula6 where

data BTree a = Empty
    | Node a (BTree a) (BTree a)
        deriving (Show)



-- /////////////////////////////////// Exercicio 1

-- a)

altura :: BTree a -> Int
altura Empty = 0
altura (Node x t1 t2) = 1 + max (altura t1) (altura t2)

--b)


contaNodes ::Num a => BTree a -> Int 
contaNodes Empty = 0                                                                     -- Se for completamente vazia, n vai contabilizar nd 
contaNodes (Node x t1 t2) = 1 + contaNodes t1 + contaNodes t2

--c)

folhas :: BTree a -> Int 
folhas Empty = 0
folhas (Node x Empty Empty) = 1
folhas (Node x t1 t2) = folhas t1 + folhas t2

--d)

prune :: Int -> BTree a -> BTree a
prune _ Empty = Empty
prune 0 _ = Empty
prune prof (Node x t1 t2) = Node x (prune (prof-1) t1) (prune (prof-1) t2)

--e)

path :: [Bool] -> BTree a -> [a]
path [] (Node x t1 t2) = [x]                                        -- Quando n ha mais esq our dir fica o q sobra
path _ Empty = []
path (h:t) (Node x t1 t2) = case h of True -> x : path t t1 
                                      False -> x : path t t2

--f)

mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node x t1 t2) = Node x (mirror t2) (mirror t1)


--g)

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT _ Empty _ = Empty
zipWithBT _ _ Empty = Empty
zipWithBT fun (Node x1 t11 t12) (Node x2 t21 t22) = Node (fun x1 x2) (zipWithBT fun t11 t21) (zipWithBT fun t12 t22)


--h)
unzipBT :: BTree (a,b,c) -> (BTree a, BTree b , BTree c)
unzipBT Empty = (Empty,Empty,Empty)
unzipBT (Node (x1,x2,x3) t1 t2) = ( Node x1 x1s1 x1s2 ,Node x2 x2s1 x2s2, Node x3 x3s1 x3s2) 
    where (x1s1,x2s1,x3s1) = unzipBT t1
          (x1s2,x2s2,x3s2) = unzipBT t2



-- //////////////////////////// Exercicio 2

--a)

minimo :: Ord a => BTree a -> a 
minimo (Node x Empty _ ) = x                    
minimo (Node _ t1 _) = minimo t1

--b)

semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node x Empty (Node x2 t1 t2)) = Node x2 t1 t2 
semMinimo (Node x t1 t2) = Node x (semMinimo t1) t2

--c)

minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node x Empty (Node x2 t1 t2)) = (x,Node x2 t1 t2 )
minSmin (Node x t1 t2) = (xs,Node x ys t2)
    where (xs,ys) = minSmin t1

--d)
remove :: Ord a => a -> BTree a -> BTree a 
remove x Empty = Empty
remove x tree@(Node y t1 t2)
    | x < y = Node x (remove x t1) t2 
    | x > y = Node x t1 (remove x t2)
    | otherwise = case t1 of Empty -> t2                    -- Quando forem iguais...
                             _ -> case t2 of Empty -> t1
                                             _ -> Node x1 t1 tree 
        where (x1,tree) = minSmin tree

-- /////////// Exercio 3

type Aluno = (Numero ,Nome ,Regime ,Classificacao)

type Numero = Int

type Nome = String

data Regime = ORD 
             | TE 
             | MEL 
    deriving Show

data Classificacao = Aprov Int
                   | Rep
                   | Faltou
    deriving Show

type Turma = BTree Aluno -- ´arvore bin´aria de procura (ordenada por n´umero)

turma :: Turma
turma = (Node (15,"Luís",ORD,Aprov 14) (Node (12,"Joana",MEL,Faltou) (Node (7,"Diogo",TE,Rep) Empty Empty) (Node (14,"Lara",ORD,Aprov 19) Empty Empty)) (Node (20,"Pedro",TE,Aprov 10) Empty (Node (25,"Sofia",ORD,Aprov 20) (Node (23,"Rita",ORD,Aprov 17) Empty Empty) (Node (28,"Vasco",MEL,Rep) Empty Empty))))

--a)

inscNum :: Numero -> Turma -> Bool
inscNum _ Empty = False
inscNum num (Node (numA,_,_,_) esq dir)
    | num == numA = True
    | num < numA = inscNum num esq
    | otherwise = inscNum num dir


--b)

inscNome :: Nome -> Turma -> Bool 
inscNome _ Empty = False
inscNome nome (Node (_,nomeA,_,_) esq dir)
    | nome == nomeA = True
    | pl1 > pl2 = inscNome nome dir
    | otherwise = inscNome nome esq
        where pl1 = head nome
              pl2 = head nomeA
        

--C)

trabEst :: Turma -> [(Numero,Nome)]
trabEst [] = []
trabEst (Node (num,nome,reg,_) esq dir) = case reg of TE = (num,nome) : trabEst