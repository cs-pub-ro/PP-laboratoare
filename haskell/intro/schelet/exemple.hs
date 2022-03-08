{-
Tipuri:

Încărcați în ghci fișierul și verificați tipul următoarelor expresii.
-}
anInteger = 42
aFloat = 42.2
aBool = False
aFunction x = x
anotherFunction x = x + 3
yetAnotherFunction x y = min x y
theLastFunction = yetAnotherFunction 3

{-
Observați:

  yetAnotherFunction :: Ord a => a -> a -> a

Contextul `Ord a` restricționează valorile variabilei de tip `a` la acele
tipuri care pot fi ordonate.

De asemenea, `theLastFunction` este o funcție `Integer -> Integer` construită
dând `yetAnotherFunction` un singur argument. Rețineți că funcțiile în Haskell
sunt curry. În final, `theLastFunction` este în formă point-free - argumentul
în care este evaluată nu apare în corpul funcției (ca în
`f x = yetAnotherFunction 3 x`).
-}

{-
Lungimea unei liste calculată în 3 feluri.
-}
length1 list = if list == [] then 0 else 1 + length1 (tail list)

length2 list = case list of
  [] -> 0
  _:xs -> 1 + length2 xs

length3 [] = 0
length3 (_:xs) = 1 + length3 xs

{-
Alte funcții simple pe liste.
-}
sumList [] = 0
sumList (x:xs) = x + sumList xs

productList [] = 1
productList (x:xs) = x * productList xs

maxList [x] = x -- nu putem avea valoare pentru lista vidă
maxList (x:xs) = max x (maxList xs)

{-
Observați că `maxList` nu poate fi apelată pentru lista vidă. Funcția nu este
totală.

De asemenea, observați că funcțiile `length3`, `sumList`, `productList` și
`maxList` urmăresc același pattern, cunoscut de la inducția structurală (AA):
un caz de bază ce întoarce o valoare și un pas inductiv tradus printr-un apel
recursiv și un apel de funcție cu 2 argumente. Șablonul este capturat de
funcțiile `fold`:
-}
lengthFold l = foldl (\x y -> x + 1) 0 l
sumListFold l = foldl (+) 0 l
productListFold l = foldl (*) 0 l
maxListFold list = foldl max (head list) (tail list)

{-
Funcția `maxListFold` poate fi scrisă point-free dar folosind construcții
avansate ce nu fac parte din scopul laboratorului curent.
-}
