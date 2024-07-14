import Prelude hiding (concat, replicate, zip, drop, take, reverse, (!!), (++), enumFromThenTo, enumFromTo, unwords, unlines, lookup)
import GHCi.FFI (C_ffi_cif)
import Data.Binary.Get (label)

-- Exercício 1
enumFromTo :: Int -> Int -> [Int]
enumFromTo x y | x > y = []
               | otherwise = x : enumFromTo (x + 1) y


-- Exercício 2 
enumFromThenTo :: Int -> Int -> Int -> [Int]
enumFromThenTo x y z | x > z = []
                     | otherwise = x : enumFromThenTo (x + (y-x)) y z


-- Exercício 3
(++) :: [a] -> [a] -> [a]
(++) l [] = l
(++) [] l = l
(++) (x : xs) y = x : (++) xs y


-- Exercício 4
(!!) :: [a] -> Int -> a
(x : xs) !! 0 = x
(x : xs) !! n | n < 0 = error "Nenhum elemento"
               | otherwise = xs !! (n-1)


-- Exercício 5
reverse :: [a] -> [a]
reverse [] = []
reverse [x] = [x]
reverse (x : xs) = reverse xs ++ [x]


-- Exercício 6
take :: Int -> [a] -> [a]
take 0 _ = []
take n [x] = [x]
take n (x:y) = x : take (n-1) y


-- Exercício 7
drop :: Int -> [a] -> [a]
drop 0 l = l
drop _ [] = []
drop n (x:y) = drop (n-1) y


-- Exercício 8
zip :: [a] -> [b] -> [(a, b)]
zip [] _          = []
zip _ []          = []
zip (x:xs) (y:ys) = (x, y) : zip xs ys


-- Exercício 9
replicate :: Int -> a -> [a]
replicate 0 a = []
replicate n a = a : replicate (n-1) a


-- Exercício 10
intersperse :: a -> [a] -> [a]
intersperse n [] = []
intersperse n (x:xs) = x : n : intersperse n xs


-- Exercício 11
group :: Eq a => [a] -> [[a]]
group [] = []
group (x : xs : t) | x == xs   = [x, xs] :  group t
                   | otherwise = [x] : group (xs : t)


-- Exercício 12
concat :: [[a]] -> [a]
concat (x : xs) = x ++ concat xs


-- Exercício 13
inits :: [a] -> [[a]]
inits xs = [take n xs | n <- [0..length xs]]


-- Exercício 14
tails :: [a] -> [[a]]
tails [] = []
tails xs = xs : tails (tail xs)


-- Exercício 15
heads :: [[a]] -> [a]
heads [] = []
heads ([] : t) = heads t
heads ((x:xs) : t) = x : heads t


-- Exercício 16
total :: [[a]] -> Int
total [] = 0
total [[x]] = 1
total (x : xs) = length x + total xs


-- Exercício 17
fun :: [(a,b,c)] -> [(a,c)]
fun [] = []
fun ((a, b, c) : t) = (a, c) : fun t


-- Exercício 18
cola :: [(String,b,c)] -> String
cola [] = []
cola ((st,b,c):t) = st ++ cola t


-- Exercício 19
idade :: Int -> Int -> [(String,Int)] -> [String]
idade _ _ [] = []
idade x1 x2 ((st,ano) : t) | x1 - ano >= x2 = st : idade x1 x2 t
                           | otherwise = st : idade x1 x2 t


-- Exercício 20
powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom n m | m < 0 = []  
                  | otherwise = n^m : powerEnumFrom n (m + 1) 


-- Exercício 21
isPrime :: Int -> Bool
isPrime n | n <= 1 = False          
          | n == 2 = True           
          | otherwise = checkDivisors 2
             where checkDivisors m | m * m > n = True      
                                   | n `mod` m == 0 = False 
                                   | otherwise = checkDivisors (m + 1)  



-- Exercício 22
-- isPrefixOf [10,20] [10,20,30]     ---> True
-- [10,30] [10,20,30]                ---> False
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = True
isPrefixOf (x:xs) (y:ys) | x == y = isPrefixOf xs ys
                         | otherwise = False


-- Exercício 23
-- isSuffixOf [20,30] [10,20,30]   ---> True
-- isSuffixOf [10,20] [10,20,30]   ---> False
isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf _ [] = False
isSuffixOf [] _ = True
isSuffixOf l (x:xs) = l == (x:xs) || isSuffixOf l xs


-- Exercício 24
-- isSubsequenceOf [20,40] [10,20,30,40]   ---> True
-- isSubsequenceOf [40,20] [10,20,30,40]   ---> False
isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (x:xs) (y:ys) = x == y && isSubsequenceOf xs ys || isSubsequenceOf (x:xs) ys


-- Exercício 25
elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices x l = [n | n <- [0..(length l - 1)], x == (l !! n)]


-- Exercício 26
nub :: Eq a => [a] -> [a]
nub [] = []
nub [x] = [x]
nub (h:t) = if h `elem` t then nub t else h:nub t


-- Exercício 27
delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete n (h:t) | n == h = t
               | otherwise = h : delete n t


-- Exercício 28
(//) :: Eq a => [a] -> [a] -> [a]
(//) l [] = l
(//) [] l = []
(//) (h:t) l | h `elem` l = t 
             | otherwise = h : (//) t l


-- Exercício 29
union :: Eq a => [a] -> [a] -> [a]
union [] l = l
union l [] = l
union l (h:t) | h `elem` l = union l t
              | otherwise = union (l ++ [h]) t


-- Exercício 30
intersect :: Eq a => [a] -> [a] -> [a]
intersect [] _ = []
intersect _ [] = []
intersect (x:xs) (y:ys) | x == y = x : intersect xs ys
                        | otherwise = intersect xs (y:ys)


-- Exercício 31
-- insert 25 [1,20,30,40] ---> [1,20,25,30,40]
insert :: Ord a => a -> [a] -> [a]
insert n [] = [n]
insert n (h:t) | n <= h = n:h:t
               | otherwise = h : insert n t 


-- Exercício 32 
-- ["Programacao", "Funcional"] ---> "Programacao Funcional".
unwords :: [String] -> String
unwords [] = ""
unwords [w] = w
unwords (w:ws) = w ++ " " ++ unwords ws


-- Exercício 33
-- unlines ["Prog", "Func"] ---> "Prog\nFunc\n"
unlines :: [String] -> String
unlines [] = ""
unlines [w] = w
unlines (w:ws) = w ++ "\n" ++ unlines ws


-- Exercício 34
{- dada uma lista não vazia, retorna a posição onde se encontra o maior elemento da lista. 
As posições da lista começam em 0 -}
pMaior :: Ord a => [a] -> Int
pMaior [x] = 0
pMaior (x:xs) = if x > head xs then pMaior xs else 1 + pMaior xs


-- Exercício 35
-- lookup ’a’ [(’a’,1),(’b’,4),(’c’,5)] ---> à lista Just 1
lookup :: Eq a => a -> [(a,b)] -> Maybe b
lookup k [] = Nothing
lookup k ((x,y):ks) | k == x = Just y
                     | otherwise = lookup k ks 


-- Exercício 36
-- preCrescente [3,7,9,6,10,22] ---> [3,7,9]
preCrescente :: Ord a => [a] -> [a]
preCrescente [] = []


-- Exercício 37
inserts :: Ord a => a -> [a] -> [a]
inserts n [] = [n]
inserts n (h:t) | n < h = n:h:t

iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort [x] = [x]
iSort (x:xs) = inserts x (iSort xs)


-- Exercício 38
-- menor "sai" "saiu" ---> True 
-- menor "programacao" "funcional" ---> False
menor :: String -> String -> Bool
menor s t | length s <= length t = True
          | otherwise = False


-- Exercicio 39
-- elemMSet ’a’ [(’b’,2), (’a’,4), (’c’,1)] ---> True 
-- elemMSet ’d’ [(’b’,2), (’a’,4), (’c’,1)] ---> False
elemMSet :: Eq a => a -> [(a,Int)] -> Bool 
elemMSet _ [] = False
elemMSet e ((k,_):ls) | e == k = True
                      | otherwise = elemMSet e ls


-- Exercício 40
-- converteMSet [(’b’,2), (’a’,4), (’c’,1)] ---> "bbaaaac"
converteMSet :: [(a, Int)] -> [a]
converteMSet [] = []
converteMSet ((x, xs):ls) = replicate xs x ++ converteMSet ls


-- Exercício 41
-- insereMSet ’c’ [(’b’,2), (’a’,4), (’c’,1)] ---> [(’b’,2), (’a’,4), (’c’,2)]
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)] 
insereMSet _ [] = []
insereMSet c ((k,v):ls) | k == c = (k, v+1) : ls
                        | otherwise = (k, v) : insereMSet c ls


-- Exercício 42
-- removeMSet ’c’ [(’b’,2), (’a’,4), (’c’,1)] ---> [(’b’,2), (’a’,4)]
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet _ [] = []
removeMSet c ((k,v):ls) | k == c = ls
                        | otherwise = (k,v) : removeMSet c ls


-- Exercício 43
-- constroiMSet "aaabccc" ---> [(’a’,3), (’b’,1), (’c’,3)]
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []


-- Exercício 44
-- Apresente uma definição recursiva da fun¸c˜ao pr´e-definida partitionEithers :: [Either a b] -> ([a],[b]) que divide uma lista de Either s em duas listas
partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers [] = ([], [])
partitionEithers (Left l : xs) =
    let (as, bs) = partitionEithers xs
    in (l : as, bs)
partitionEithers (Right r : xs) =
    let (as, bs) = partitionEithers xs
    in (as, r : bs)


-- Exercício 45
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing : ms) = catMaybes ms
catMaybes (Just x : ms) = x : catMaybes ms


-- Exercício 46
data Movimento = Norte | Sul | Este | Oeste
    deriving Show

caminho :: (Int, Int) -> (Int, Int) -> [Movimento]
caminho (x, y) (xs, ys) | x < xs    = Oeste : caminho (x+1, y) (xs, ys)
                        | x > xs    = Este : caminho (x-1, y) (xs, ys)
                        | y < ys    = Norte : caminho (x, y+1) (xs, ys)
                        | y > ys    = Sul : caminho (x, y-1) (xs, ys)
                        | otherwise = []


-- Exercício 47
type Posicao = (Int, Int)

movimento :: Posicao -> Movimento -> Posicao
movimento (x, y) Norte = (x, y + 1)
movimento (x, y) Sul   = (x, y - 1)
movimento (x, y) Este  = (x + 1, y)
movimento (x, y) Oeste = (x - 1, y)

listaElem :: Eq a => a -> [a] -> Bool
listaElem _ [] = False
listaElem x (y:ys) = x == y || listaElem x ys

hasLoops :: Posicao -> [Movimento] -> Bool
hasLoops _ [] = False
hasLoops pos (mov:movimentos) =
    let novaPos = movimento pos mov
    in novaPos == pos || listaElem novaPos (percurso novaPos movimentos)
  where
    percurso :: Posicao -> [Movimento] -> [Posicao]
    percurso _ [] = []
    percurso p (m:ms) =
        let novaPos = movimento p m
        in novaPos : percurso novaPos ms


-- Exercício 48
type Ponto = (Float, Float)
data Rectangulo = Rect Ponto Ponto 
     deriving Show

difPonto :: Ponto -> Ponto -> (Float, Float)
difPonto (x1, y1) (x2, y2) = (abs (x2 - x1), abs (y2 - y1))

eQuadrado :: Rectangulo -> Bool
eQuadrado (Rect p1 p2) = difPonto p1 p2 == (difPonto (0, 0) p2)

contaQuadrados :: [Rectangulo] -> Int
contaQuadrados = length . filter eQuadrado


-- Exercício 49
areaRetangulo :: Rectangulo -> Float
areaRetangulo (Rect (x1, y1) (x2, y2)) = abs ((x2 - x1) * (y2 - y1))

areaTotal :: [Rectangulo] -> Float
areaTotal = sum . map areaRetangulo


-- Exercício 50
data Equipamento = Bom | Razoavel | Avariado 
       deriving Show

instance Eq Equipamento where
  Bom == Bom = True
  Razoavel == Razoavel = True
  Avariado == Avariado = True
  _ == _ = False

naoReparar :: [Equipamento] -> Int
naoReparar equipamentos = length (filter (\estado -> estado == Bom || estado == Razoavel) equipamentos)
