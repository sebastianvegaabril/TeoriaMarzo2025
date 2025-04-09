module Main where

----------------------------------------------------------------------------------------------------
---- Cambios necesarios para implementar la potencia de un conjunto y la igualdad de conjuntos -----
----------------------------------------------------------------------------------------------------
---- Esto es una posible solucion que se me ocurrio a mi, no quiere decir que si lo hicieron -------
-------- diferente este mal. Si no entienden algo o algo no le convence no duden -------------------
--------------------------------- en preguntarme por teams. 😃 -------------------------------------
-------------------------------------- (Sebastian Vega) --------------------------------------------
----------------------------------------------------------------------------------------------------
------------ La solucion tiene pretty printing para ver de una forma mas clara los tests -----------
----------------------------- mas abajo se explica como usarlo. ------------------------------------
----------------------------------------------------------------------------------------------------

import Prelude hiding (nub)
import Data.List (nub, intercalate)

-- Primero tenemos que extender el data para incluir el 
-- conjunto potencia y la igualdad de conjuntos.

data E = 
        Pot E
    |   Equal E E
    |   Var X
    |   Empty
    |   Unit Int
    |   Pert Int E
    |   Union E E
    |   Inter E E
    |   Diff E E
    |   Incl E E
    |   Asig X E deriving Show

type X = String

-- Hay que extender los valores tambien, porque ahora 
-- nos vamos a encontrar los conjuntos de conjuntos
-- como retorno de Pot.
-- Hay que tener en cuenta que Pot no solo es la potencia
-- de un conjunto de entero. Puede haber potencia
-- de potencia de potencia de un conjunto de enteros, por
-- ejemplo. Entonces se me ocurrio esta idea, separar
-- la lista de enteros de los conjuntos, para que existan
-- conjuntos de conjuntos, y tambien conjuntos de enteros.
-- Como mencione mas arriba, esta no es la unica solucion
-- que existe, es una de las posibles.

data V = Z [Int] | B Bool | Conj [V] deriving (Eq, Show)

type M = [(X, V)]

-- Ahora debemos hacer las funciones auxiliares que
-- nos permitan encontrar la potencia de un conjunto,
-- y otra que compare los conjuntos.

potencia :: [a] -> [[a]]
potencia [] = [[]]
potencia (x:xs) = potencia xs ++ map (x:) (potencia xs) -- Explicacion de esta funcion: Lo voy a explicar con el ejemplo [1,2]
--                                                                                      primero hago la potencia de 2, que da [[],[2]]
--                                                                                      despues a cada uno de esos conjuntos le agrego el 1
--                                                                                      y queda [[1], [1,2]]. Ahora le concateno esto a la 
--                                                                                      que calcule al principio, dando por resultado 
--                                                                                      [[],[2],[1],[1,2]] que es el resultado esperado,
--                                                                                      la potencia de [1,2].

equal :: V -> V -> Bool
equal (Z xs) (Z ys) = xs == ys
equal (B b1) (B b2) = b1 == b2
equal (Conj vs) (Conj vs') = length vs == length vs' && all (`elem` vs') vs -- <- Si son del mismo largo, y todos los de uno estan en el otro, son iguales.
equal _ _ = False

-- Tambien debemos cambiar las auxiliares que
-- ya teniamos para usar los nuevos valores
-- que tenemos. Los que mas cambian son los 
-- comparan, porque ya no tenemos listas de enteros
-- ahora tenemos listas de valores.

lkup :: X -> M -> V
lkup x [] = error "Variable no definida."
lkup x ((x',v):m')
    | x == x' = v
    | otherwise = lkup x m'

upd :: M -> (X,V) -> M
upd [] xv = [xv]
upd (xv@(x,v):m') xv'@(x',v')
    | x == x' = xv':m'
    | otherwise = xv:(upd m' xv')

belongs :: Int -> [V] -> Bool
belongs z [] = False
belongs z (v:vs) = case v of {
    Z zs -> if elem z zs then True else belongs z vs;
    _ -> belongs z vs
 }

union :: [V] -> [V] -> [V]
union c1 c2 = nub (c1 ++ c2)

intersection :: [V] -> [V] -> [V]
intersection [] c2 = []
intersection (x:xs) c2
    | elem x c2 = x:(intersection xs c2)
    | otherwise = intersection xs c2

difference :: [V] -> [V] -> [V]
difference [] c2 = []
difference (x:xs) c2
    | not (elem x c2) = x:(difference xs c2)
    | otherwise = difference xs c2

belongsConj :: V -> [V] -> Bool
belongsConj _ [] = False
belongsConj x (v:vs)
    | equal x v = True
    | otherwise = belongsConj x vs

included :: [V] -> [V] -> Bool
included (x:xs) c2 = belongsConj x c2 && included xs c2
included _ v = True

-- Por ultimo, debemos extender el eval para que se 
-- puedan evaluar expresiones de potencia e igualdad.
-- Y hacer los cambios necesarios en los casos
-- que ya teniamos hechos para usar los nuevos valores.

eval :: (M,E) -> (M,V)
eval (m, Var x) = (m, lkup x m)
eval (m, Empty) = (m, Conj [])
eval (m, Unit z) = (m,Conj [Z [z]])
eval (m, Pot e) = case eval (m,e) of {
    (m', Conj [Z xs]) -> (m', Conj (map Z (potencia xs)));
    (m', Conj vs) -> (m', Conj (map Conj (potencia vs)))
} 
eval (m, Union e1 e2) = case eval (m,e1) of {
    (m', Conj c1) -> case eval (m',e2) of{
        (m'', Conj c2) -> (m'', Conj (union c1 c2))
    }
}
eval (m, Pert z e) = case eval (m,e) of {
        (m', Conj c) -> (m', B (belongs z c))
}
eval (m, Inter e1 e2) = case eval (m,e1) of {
    (m', Conj c1) -> case eval (m',e2) of{
        (m'', Conj c2) -> (m'', Conj (intersection c1 c2))
    }
}
eval (m, Diff e1 e2) = case eval (m,e1) of {
    (m', Conj c1) -> case eval (m',e2) of {
        (m'', Conj c2) -> (m'', Conj (difference c1 c2))
    }
}
eval (m, Asig x e) = case eval (m,e) of {
    (m', v) -> (upd m' (x,v), v)
}
eval (m, Incl e1 e2) = case eval (m,e1) of {
    (m', Conj c1) -> case eval (m',e2) of {
        (m'', Conj c2) -> (m'', B (included c1 c2))
    }
}
eval (m, Equal e1 e2) = case eval (m,e1) of {
    (m', Conj c1) -> case eval (m',e2) of {
        (m'', Conj c2) -> (m'', B (equal (Conj c1) (Conj c2)))
    }
}

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
--------------------------------------------- Tests ------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- Tests de Union
testUnion1 :: (M, V)
testUnion1 = eval ([], Union (Unit 1) (Unit 2))

testUnion2 :: (M, V)
testUnion2 = eval ([], Union (Unit 1) (Unit 1))

testUnion3 :: (M, V)
testUnion3 = eval ([], Union (Union (Unit 1) (Unit 2)) (Unit 3))

testUnion4 :: (M, V)
testUnion4 = eval ([], Union Empty (Unit 1))

testUnion5 :: (M, V)
testUnion5 = eval ([], Union Empty Empty)

-- Tests de Pot
testPot1 :: (M, V)
testPot1 = eval ([], Pot (Unit 1))

testPot2 :: (M, V)
testPot2 = eval ([], Pot (Union (Unit 1) (Unit 2)))

testPot3 :: (M, V)
testPot3 = eval ([], Pot (Union (Unit 1) (Union (Unit 2) (Unit 3))))

testPot4 :: (M, V)
testPot4 = eval ([], Pot (Pot (Unit 1)))

testPot5 :: (M, V)
testPot5 = eval ([], Pot (Pot (Union (Unit 1) (Unit 2))))

-- Tests de Pert
testPert1 :: (M, V)
testPert1 = eval ([], Pert 1 (Unit 1))

testPert2 :: (M, V)
testPert2 = eval ([], Pert 2 (Unit 1))

testPert3 :: (M, V)
testPert3 = eval ([], Pert 2 (Union (Unit 1) (Unit 2)))

testPert4 :: (M, V)
testPert4 = eval ([], Pert 3 (Union (Unit 1) (Unit 2)))

-- Tests de Inter
testInter1 :: (M, V)
testInter1 = eval ([], Inter (Unit 1) (Unit 1))

testInter2 :: (M, V)
testInter2 = eval ([], Inter (Unit 1) (Unit 2))

testInter3 :: (M, V)
testInter3 = eval ([], Inter (Union (Unit 1) (Unit 2)) (Union (Unit 2) (Unit 3)))

-- Tests de Diff
testDiff1 :: (M, V)
testDiff1 = eval ([], Diff (Unit 1) (Unit 1))

testDiff2 :: (M, V)
testDiff2 = eval ([], Diff (Union (Unit 1) (Unit 2)) (Unit 2))

testDiff3 :: (M, V)
testDiff3 = eval ([], Diff (Union (Unit 1) (Unit 2)) (Union (Unit 2) (Unit 3)))

-- Tests de Incl
testIncl1 :: (M, V)
testIncl1 = eval ([], Incl (Unit 1) (Union (Unit 1) (Unit 2)))

testIncl2 :: (M, V)
testIncl2 = eval ([], Incl (Unit 3) (Union (Unit 1) (Unit 2)))

testIncl3 :: (M, V)
testIncl3 = eval ([], Incl (Union (Unit 1) (Unit 2)) (Union (Unit 1) (Unit 2)))

testIncl4 :: (M, V)
testIncl4 = eval ([], Incl (Union (Unit 1) (Unit 3)) (Union (Unit 1) (Unit 2)))

testIncl5 :: (M, V)
testIncl5 = eval ([], Incl Empty (Unit 1))

testIncl6 :: (M, V)
testIncl6 = eval ([], Incl (Union (Unit 1) (Unit 2)) (Union (Unit 1) (Union (Unit 2) (Unit 3))))

-- Tests de Equal
testEqual1 :: (M, V)
testEqual1 = eval ([], Equal (Union (Unit 1) (Unit 2)) (Union (Unit 2) (Unit 1)))

testEqual2 :: (M, V)
testEqual2 = eval ([], Equal (Union (Unit 1) (Unit 2)) (Union (Unit 1) (Unit 3)))

testEqual3 :: (M, V)
testEqual3 = eval ([], Equal (Unit 1) (Unit 1))

testEqual4 :: (M, V)
testEqual4 = eval ([], Equal (Unit 1) (Unit 2))

testEqual5 :: (M, V)
testEqual5 = eval ([], Equal Empty (Unit 1))

testEqual6 :: (M, V)
testEqual6 = eval ([], Equal Empty Empty)

testEqual7 :: (M, V)
testEqual7 = eval ([], Equal (Union (Unit 1) (Unit 2)) (Union (Unit 1) (Union (Unit 2) (Unit 3))))

----------------------------------------------------------------------------------------------------
----------------------------------------- Pretty printing ------------------------------------------
----------------------------------------------------------------------------------------------------
-------------------------- No es parte de la solucion, es para verlo bien. -------------------------
----------------------------------------------------------------------------------------------------
-------------- Nunca se les va a pedir hacer esto, pero si quieren saber como hacerlo --------------
---------------------- pueden tomar este de ejemplo, o preguntarme y los ayudo ---------------------
----------------------------------------------------------------------------------------------------
------------------- Si quieren probarlo con pretty printing ejecutar los comandos: -----------------
------------------------- ghc Practico0Extendido.hs -o Practico0Extendido --------------------------
---------------------------------- Practico0Extendido <-Windows ------------------------------------
-------------------------------- ./Practico0Extendido <-Linux/Mac ----------------------------------
----------------------------------------------------------------------------------------------------

prettyPrint :: V -> String
prettyPrint (Z []) = "{}"
prettyPrint (Z xs) = intercalate ", " (map show xs)
prettyPrint (B b) = show b
prettyPrint (Conj vs) = "{" ++ intercalate ", " (map prettyPrint vs) ++ "}"

prettyPrintResult :: (M, V) -> String
prettyPrintResult (_, v) = prettyPrint v

main :: IO ()
main = do
    putStrLn "Tests de Union:"
    putStrLn $ "Union de {1} y {2}: " ++ prettyPrintResult testUnion1
    putStrLn $ "Union de {1} y {1}: " ++ prettyPrintResult testUnion2
    putStrLn $ "Union de {1, 2} y {3}: " ++ prettyPrintResult testUnion3
    putStrLn $ "Union de {} y {1}: " ++ prettyPrintResult testUnion4
    putStrLn $ "Union de {} y {}: " ++ prettyPrintResult testUnion5

    putStrLn "\nTests de Pot:"
    putStrLn $ "Potencia de {1}: " ++ prettyPrintResult testPot1
    putStrLn $ "Potencia de {1, 2}: " ++ prettyPrintResult testPot2
    putStrLn $ "Potencia de {1, 2, 3}: " ++ prettyPrintResult testPot3
    putStrLn $ "Potencia de una potencia de {1}: " ++ prettyPrintResult testPot4
    putStrLn $ "Potencia de una potencia de {1, 2}: " ++ prettyPrintResult testPot5

    putStrLn "\nTests de Pert:"
    putStrLn $ "1 pertenece a {1}? " ++ prettyPrintResult testPert1
    putStrLn $ "2 pertenece a {1}? " ++ prettyPrintResult testPert2
    putStrLn $ "2 pertenece a {1, 2}? " ++ prettyPrintResult testPert3
    putStrLn $ "3 pertenece a {1, 2}? " ++ prettyPrintResult testPert4

    putStrLn "\nTests de Inter:"
    putStrLn $ "Interseccion de {1} y {1}: " ++ prettyPrintResult testInter1
    putStrLn $ "Interseccion de {1} y {2}: " ++ prettyPrintResult testInter2
    putStrLn $ "Interseccion de {1, 2} y {2, 3}: " ++ prettyPrintResult testInter3

    putStrLn "\nTests de Diff:"
    putStrLn $ "Diferencia de {1} y {1}: " ++ prettyPrintResult testDiff1
    putStrLn $ "Diferencia de {1, 2} y {2}: " ++ prettyPrintResult testDiff2
    putStrLn $ "Diferencia de {1, 2} y {2, 3}: " ++ prettyPrintResult testDiff3

    putStrLn "\nTests de Incl:"
    putStrLn $ "{1} esta incluido en {1, 2}? " ++ prettyPrintResult testIncl1
    putStrLn $ "{3} esta incluido en {1, 2}? " ++ prettyPrintResult testIncl2
    putStrLn $ "{1, 2} esta incluido en {1, 2}? " ++ prettyPrintResult testIncl3
    putStrLn $ "{1, 3} esta incluido en {1, 2}? " ++ prettyPrintResult testIncl4
    putStrLn $ "{} esta incluido en {1}? " ++ prettyPrintResult testIncl5
    putStrLn $ "{1, 2} esta incluido en {1, 2, 3}? " ++ prettyPrintResult testIncl6

    putStrLn "\nTests de Equal:"
    putStrLn $ "{1, 2} es igual a {2, 1}? " ++ prettyPrintResult testEqual1
    putStrLn $ "{1, 2} es igual a {1, 3}? " ++ prettyPrintResult testEqual2
    putStrLn $ "{1} es igual a {1}? " ++ prettyPrintResult testEqual3
    putStrLn $ "{1} es igual a {2}? " ++ prettyPrintResult testEqual4
    putStrLn $ "{} es igual a {1}? " ++ prettyPrintResult testEqual5
    putStrLn $ "{} es igual a {}? " ++ prettyPrintResult testEqual6
    putStrLn $ "{1, 2} es igual a {1, 2, 3}? " ++ prettyPrintResult testEqual7

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
---------------------------------------------- FIN -------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------