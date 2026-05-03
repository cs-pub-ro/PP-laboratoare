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

DERIVARE:

Funcția care se aplică lui xs este: foldr u c . (++ ys). În acest moment,
putem rescrie (++ ys) ca foldr (:) ys, întreaga expresie devenind:

foldr u c . foldr (:) ys.

Acum, putem aplica proprietatea de fuziune a foldr. Instanțiem variabilele din 
proprietate astfel:

h = foldr u c
f = (:)
a = ys

Condițiile sunt următoarele:

b = h a = foldr u c ys

h (f x y) = g x (h y)
foldr u c (x : y) = g x (foldr u c y)
u x (foldr u c y) = g x (foldr u c y)
g x acc = u x acc
g = u

Forma finală a expresiei originale este:

foldr g b = foldr u (foldr u c ys) xs,

care ne spune că putem reduce mai întâi ys pornind de la acumulatorul inițial 
c, și apoi utiliza rezultatul ca acumulator inițial pentru reducerea lui xs.
-}
foldAppend :: (a -> b -> b) -> b -> [a] -> [a] -> b
foldAppend u c xs ys = foldr u (foldr u c ys) xs

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

DERIVARE:

Rescriem concat ca foldr (++) [], întreaga expresie devenind:

foldr u c . foldr (++) [].

Acum, putem aplica proprietatea de fuziune a foldr. Instanțiem variabilele din 
proprietate astfel:

h = foldr u c
f = (++)
a = []

Condițiile sunt următoarele:

b = h a = foldr u c [] = c

h (f x y) = g x (h y)
foldr u c (x ++ y) = g x (foldr u c y)

Rescriem partea stângă utilizând ex. 1:

foldr u (foldr u c y) x = g x (foldr u c y)
g x acc = foldr u acc x
g x acc = flip (foldr u) x acc
g = flip (foldr u)

Forma finală a funcției originale este:

foldr (flip (foldr u)) c,

care ne spune că pentru fiecare listă internă întâlnită, se poate desfășura un 
foldr intern care reduce acea listă la un singur acumulator, oferit foldr-ului
extern.
-}

foldConcat :: (a -> b -> b) -> b -> [[a]] -> b
foldConcat u c xss = foldr (flip (foldr u)) c xss
-- Definiții alternative:
-- foldConcat u = foldr (flip (foldr u))
-- foldConcat = foldr . flip . foldr

check2 :: TestData
check2 = tests_ 2
    [
        testVal "foldConcat 1" [1, 2, 3, 4, 5, 6, 7, 8] $
            foldConcat (:) [] [[1, 2, 3], [4, 5, 6], [7, 8]],
        testVal "foldConcat 2" 36 $
            foldConcat (+) 0 [[1, 2, 3], [4, 5, 6], [7, 8]]
    ]

check = quickCheck False [check1, check2]