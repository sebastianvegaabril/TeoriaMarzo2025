module Ejercicios where

import Imp

not' :: Program
not' = Case "b" [ 
        ("True", ([], Asig [("result", K "False" [])])), 
        ("False", ([], Asig [("result", K "True" [])]))
 ]

testNotTrue :: M
testNotTrue = exec [] (Sec (Asig [("b", K "True" [])]) not')

testNotFalse :: M
testNotFalse = exec [] (Sec (Asig [("b", K "False" [])]) not')

esPar :: Program
esPar = Sec 
            (Asig [("result", K "True" [])]) 
            (Local ["n'"] (
                Sec 
                    (Asig [("n'", Var "n")]) 
                    (While "n'" [
                        ("S",(["x"], Sec 
                                        (Case "result" [
                                            ("True",([], Asig [("result", K "False" [])])),
                                            ("False",([], Asig [("result", K "True" [])]))
                                        ]) 
                                        (Asig [("n'", Var "x")])
                            ))
                    ])
            ))

testEsParCon1 :: M
testEsParCon1 = exec [] (Sec (Asig [("n",K "S" [K "O" []])]) esPar) 

testEsParCon2 :: M
testEsParCon2 = exec [] (Sec (Asig [("n",K "S" [K "S" [K "O" []]])]) esPar) 

contarUnos :: Program
contarUnos = Sec 
                (Asig [("result", K "O" [])])
                (Local ["l'"] (
                    Sec
                        (Asig [("l'", Var "l")])
                        (While "l'" [
                            (":",(["x","xs"], Sec 
                                                (Case "x" [
                                                    ("S",(["O"], Asig [("result", K "S" [Var "result"])])),
                                                    ("O",([], Asig [("result", Var "result")]))
                                                ])
                                                (Asig [("l'", Var "xs")])
                            ))
                        ])
                ))


testContarUnosCon0Unos :: M
testContarUnosCon0Unos = exec [] (Sec (Asig [("l", K ":" [K "O" [], K "O" []])]) contarUnos)

testContarUnosCon1Uno :: M
testContarUnosCon1Uno = exec [] (Sec (Asig [("l", K ":" [K "S" [K "O" []], K "O" []])]) contarUnos)

testContarUnosCon2Unos :: M
testContarUnosCon2Unos =  exec [] (Sec (Asig [("l", K ":" [K "S" [K "O" []], K ":" [K "O" [], K ":" [K "S" [K "O" []], K "O" []]]])]) contarUnos)
