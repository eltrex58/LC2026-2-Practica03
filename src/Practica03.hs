{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{- HLINT ignore "Use camelCase" -}
module Practica03 where

--Sintaxis de la logica proposicional
data Prop = Var String | Cons Bool | Not Prop
            | And Prop Prop | Or Prop Prop
            | Impl Prop Prop | Syss Prop Prop
            deriving (Eq)

instance Show Prop where
                    show (Cons True) = "⊤"
                    show (Cons False) = "⊥"
                    show (Var p) = p
                    show (Not p) = "¬" ++ show p
                    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
                    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
                    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
                    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"
w = Var "w"
v = Var "v"

{-
FORMAS NORMALES
-}

--Ejercicio 1
fnn :: Prop -> Prop
fnn (Cons True) = Cons True
fnn (Cons False) = Cons False
fnn (Var a) = Var a
fnn (Not a) = negar (fnn a)
fnn (Impl a b) = Or (fnn (Not a)) (fnn b)
fnn (Syss a b) = And (fnn (Impl a b)) (fnn (Impl b a))
fnn (Or a b) = Or (fnn a) (fnn b)
fnn (And a b) = And (fnn a) (fnn b)

-- Aplica la negación a una prop
negar :: Prop -> Prop
negar (Cons True) = Cons False
negar (Cons False) = Cons True
negar (Var a) = Not (Var a)
negar (Not a) = a
negar (Or a b) = And (negar a) (negar b)
negar (And a b) = Or (negar a) (negar b)
negar (Impl a b) = And a (negar b)
negar (Syss a b) = negar (And (Impl a b) (Impl b a))

--Ejercicio 2
fnc :: Prop -> Prop
fnc prop = fncAux (fnn prop)

-- Hace todo lo que haria fnc
fncAux :: Prop -> Prop
fncAux (Cons True) = Cons True
fncAux (Cons False) = Cons False
fncAux (Var a) = Var a
fncAux (Not a) = Not a
fncAux (And a b) = And (fncAux a) (fncAux b)
fncAux (Or a b) = dist (fncAux a) (fncAux b)

-- Distribuye propiamente los OR
dist :: Prop -> Prop -> Prop
dist (And a b) c = And (dist a c) (dist b c)
dist a (And b c) = And (dist a b) (dist a c)
dist a b = Or a b

{-
RESOLUCION BINARIA
-}

--Sinonimos a usar
type Literal = Prop
type Clausula = [Literal]

--Ejercicio 1
--NOTA: Quita clausulas repetidas
clausulas :: Prop -> [Clausula]
clausulas (Cons True) = [[Cons True]]
clausulas (Cons False) = [[Cons False]]
clausulas (Var a) = [[Var a]]
clausulas (Not a) = [[Not a]]
clausulas (Or a b) = unionInt (clausulas a) (clausulas b)
clausulas (And a b) = union (clausulas a) (clausulas b)
clausulas _ = [[]]

-- Une las listas internas de dos listas de listas, generando un conjunto interno.
unionInt :: Eq a =>[[a]] -> [[a]] -> [[a]]
unionInt [x] [y] = [union x y]

-- Une dos listas de elementos y genera un conjunto.
union :: Eq a => [a] -> [a] -> [a]
union ys [] = ys
union ys (x:xs)
    | elemento x ys = union ys xs
    | otherwise      = union (ys ++ [x]) xs

-- Funcion que verifica si un elemento pertenece a una lista.
elemento :: Eq a => a -> [a] -> Bool
elemento _ [] = False
elemento x (y:ys) = x == y || elemento x ys


--Ejercicio 2
-- Devuelve la unión de las dos clausulas si no es posible.
resolucion :: Clausula -> Clausula -> Clausula
resolucion [] x = x
resolucion (x:xs) (y:ys) =
    let r = res x (y:ys)
    in if r == y:ys
        then union [x]  (resolucion xs (y:ys))
        else r ++ xs

-- Hace una resolucion binaria de una literal con una clausula. Devuelve la clausula entera si no es posible.
res :: Literal -> Clausula -> Clausula
res _ [] = []
res x (y:ys)
    | x == negar y = ys
    | otherwise = y : res x ys



{--
rs :: [Clausula] -> [Clausula]
rs [] = []
rs [x] = [x]
rs (x:(y:xs)) = 
    if hayResolvente x y 
        then (x:(y:xs)) ++ [resolucion x y] ++ rs (x:xs) ++ rs (y:xs) 
        else (x:(y:xs)) ++ rs (x:xs) ++ rs (y:xs) 
--}
{-
ALGORITMO DE SATURACION
-}

--Ejercicio 1
hayResolvente :: Clausula -> Clausula -> Bool
hayResolvente [] _ = False
hayResolvente (x:xs) (y:ys) =
    let r = res x (y:ys)
    in (not (r ==  (y:ys))) || hayResolvente xs (y:ys)

---------------------------- Ejercicio 2 --------------------------------

--Funcion principal que pasa la formula proposicional a fnc e invoca a res con las clausulas de la formula.
saturacion :: Prop -> Bool
saturacion f = notB (saturacionAux 100 (clausulas (fnc f))) --negamos pues se quiere sabes si la función es satisfacible y lo que nos da el algoritmo es si es insatisfacible.
{-saturacion f = 
    let s = clausulas (fnc f)
    in  saturacionCiclo 50000 s -}

-- Función not
notB :: Bool -> Bool
notB True = False
notB False = True

-- Checa los casos del algorimo de saturacion, devuelve si la la prop es insatisfacible o no
saturacionAux :: Int -> [Clausula] -> Bool
saturacionAux 0 _ = error "se acabaron los recursos xd"  -- termina cuando llega a 0, esto representa como los recursos
saturacionAux n s
    | contieneVacia s = True   -- caso 1: [] ∈ Resn(S) 
    | sNext == s      = False  -- caso 2: Resn-1 = Resn(S), notese que acá se construye fR s = Res(n)
    | otherwise       = saturacionAux (n-1) sNext 
    where sNext = fR s

-- Verifica si la cláusula vacía [] está presente
contieneVacia :: [Clausula] -> Bool
contieneVacia s = elemento [] s

-- Función R: Calcula la unión de S con todos sus resolventes
fR :: [Clausula] -> [Clausula]
fR s = union s (resolventes s)

-- Genera todos los resolventes posibles entre pares de cláusulas
resolventes :: [Clausula] -> [Clausula]
resolventes [] = []
resolventes (c:cs) = union (resolventeAux c cs) (resolventes cs)

-- Genera los resolventes de una cláusula contra una lista
resolventeAux :: Clausula -> [Clausula] -> [Clausula]
resolventeAux _ [] = []
resolventeAux c (x:xs) = 
    if hayResolvente c x 
    then union [resolucion c x] (resolventeAux c xs)
    else resolventeAux c xs


