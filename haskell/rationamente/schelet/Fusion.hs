module Fusion where

import TestPP

{-
Ex. 1.

Utilizați equational reasoning în tandem cu proprietatea de fuziune a foldr, 
pentru a rescrie expresia următoare fără a recurge la concatenare:

foldr u c (xs ++ ys).

Scopul este de a acumula direct elementele celor liste, fără concatenarea
prealabilă. Evaluarea leneșă din Haskell anulează în principiul costul 
concatenării, dar în cazul evaluării eager, ca în Racket, câștigul este 
important, pentru că nu se mai parcurge de două ori lista xs (o dată pentru 
concatenare și o dată pentru foldr).

Hints:

1. Izolați funcția care se aplică lui xs. Scrieți-o în stil point-free, 
   utilizând compunerea de funcții.
2. Rescrieți-o pe aceasta astfel încât proprietatea de fuziune să devină 
   aplicabilă.

DERIVARE: ...
-}
foldAppend :: (a -> b -> b) -> b -> [a] -> [a] -> b
foldAppend u c xs ys = undefined

check1 :: TestData
check1 = tests_ 1 
    [
        testVal "foldAppend 1" [1, 2, 3, 4, 5, 6] $
            foldAppend (:) [] [1, 2, 3] [4, 5, 6],
        testVal "foldAppend 2" 21 $
            foldAppend (+) 0 [1, 2, 3] [4, 5, 6]
    ]

{-
Ex. 2.

Utilizați equational reasoning în tandem cu proprietatea de fuziune a foldr, 
pentru a rescrie funcția următoare, având tipul [[a]] -> b, fără a recurge
la concatenare:

foldr u c . concat.

Scopul este de a acumula direct elementele listelor interne, fără liniarizare
prealabilă.

Hint: Rescrieți concat astfel încât proprietatea de fuziune să devină 
aplicabilă.

DERIVARE: ...
-}

foldConcat :: (a -> b -> b) -> b -> [[a]] -> b
foldConcat u c xss = undefined

check2 :: TestData
check2 = tests_ 2
    [
        testVal "foldConcat 1" [1, 2, 3, 4, 5, 6, 7, 8] $
            foldConcat (:) [] [[1, 2, 3], [4, 5, 6], [7, 8]],
        testVal "foldConcat 2" 36 $
            foldConcat (+) 0 [[1, 2, 3], [4, 5, 6], [7, 8]]
    ]

check = quickCheck False [check1, check2]