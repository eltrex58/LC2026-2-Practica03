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
fnn (Impl a b) = Or (Not (fnn a)) (fnn b)
fnn (Syss a b) = And (fnn (Impl a b)) (fnn (Impl b a))
fnn (Or a b) = Or (fnn a) (fnn b)
fnn (And a b) = And (fnn a) (fnn b)

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
fnc (Cons True) = Cons True
fnc (Cons False) = Cons False
fnc (Var a) = Var a
fnc (Not a) = dist (fnn a)
fnc (And a b) = dist (And a b)
fnc (Or a b) = dist (Or a b)
fnc (Impl a b) = dist (fnn (Impl a b))
fnc (Syss a b) = dist (fnn (Syss a b))


dist :: Prop -> Prop
dist (Or (And b c) a) = And (dist (Or b a)) (dist (Or c a))
dist (Or a (And b c)) = And (dist (Or a b)) (dist (Or a c))
dist (And a b) = And (dist a) (dist b)
dist (Or a b) = Or (dist a) (dist b)
dist p = p

{-
RESOLUCION BINARIA
-}

--Sinonimos a usar
type Literal = Prop
type Clausula = [Literal]

--Ejercicio 1
clausulas :: Prop -> [Clausula]
clausulas (Cons True) = [[Cons True]]
clausulas (Cons False) = [[Cons False]]
clausulas (Var a) = [[Var a]]
clausulas (Not a) = [[Not a]]
clausulas (Or a b) = [a:[b]]
clausulas (And a b) = union (clausulas a) (clausulas b)
clausulas _ = [[]]

union :: Eq a => [a] -> [a] -> [a]
union ys [] = ys
union ys (x:xs)
    | elem x ys = union ys xs
    | otherwise      = union (ys ++ [x]) xs


--Ejercicio 2
resolucion :: Clausula -> Clausula -> Clausula
resolucion [] _ = []
resolucion (x:xs) (y:ys) =
    let r = res x (y:ys)
    in if null r
        then x : resolucion xs (y:ys)
        else r

res :: Literal -> Clausula -> Clausula
res _ [] = []
res x (y:ys)
    | x == negar y = ys
    | otherwise = res x ys



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
    in (not (null r) || hayResolvente xs (y:ys))

--Ejercicio 2
--Funcion principal que pasa la formula proposicional a fnc e invoca a res con las clausulas de la formula.
saturacion :: Prop -> Bool
saturacion = undefined