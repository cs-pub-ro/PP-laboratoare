{-
  PP, laboratorul 7: tipuri de date utilizator
-}

import Data.List
import Data.Maybe
import TestPP

{-
  1. Vectori
  Se dă tipul de date Vector, reprezentând vectori din spațiul R^3.

  Implementați următoarele operații cu vectori:
  - norma unui vector
  - produsul scalar (dot product) dintre doi vectori
  - adunarea a doi vectori
  - scăderea a doi vectori

  Explicații

  Fie a și b doi vectori din R^3 considerați de forma:
  a = a1 * i + a2 * j + a3 * k
  b = b1 * i + b2 * j + b3 * k
  Produsul scalar al celor doi vectori o să fie egal cu:
  a • b = a1 * b1 + a2 * b2 + a3 * b3

  Pentru mai multe detalii, consultați:
  https://gerardnico.com/linear_algebra/vector_vector
-}
data Vector = V
  { vx :: Double
  , vy :: Double
  , vz :: Double
  } deriving (Show, Eq)

lengthV :: Vector -> Double
lengthV (V vx vy vz) = sqrt $ vx * vx + vy * vy + vz * vz

dotV :: Vector -> Vector -> Double
dotV (V vx1 vy1 vz1) (V vx2 vy2 vz2) = vx1 * vx2 + vy1 * vy2 + vz1 * vz2

addV :: Vector -> Vector -> Vector
addV (V vx1 vy1 vz1) (V vx2 vy2 vz2) = V (vx1 + vx2) (vy1 + vy2) (vz1 + vz2)

subV :: Vector -> Vector -> Vector
subV (V vx1 vy1 vz1) (V vx2 vy2 vz2) = V (vx1 - vx2) (vy1 - vy2) (vz1 - vz2)


check1 :: TestData
check1 = let 
      v1 = V 1 (-1) 0
      v2 = V 1 1 0
      v3 = V 0 0 0
  in tests_ 1 $
          [ testVal "lengthV" (sqrt 2) $ lengthV v1
          , testVal "dotV" 0.0 $  dotV v1 v2
          , testVal "addV" (V 2 0 0) $ addV v1 v2
          , testVal "subV" v2 $ subV v2 v3
          ]

{-
 2. Liste imbricate

  Definiți un tip de date SList a care să aibă funcționalități
  asemănătoare listelor din limbajele Lisp (e.g. Scheme, Racket, Clojure),
  permițând componente la diferite niveluri de imbricare.

  Ex: Lista din Racket '(1 (3 4) (2)) să poată fi definită în Haskell
  folosind SList.

  Adițional, definiți:
  - emptySList, lista vidă
  - consElem, adaugă un element în capul unei liste
    Ex: consElem 1 '((3 4) (2)) == '(1 (3 4) (2))
  - consList, adaugă o listă (imbricată) în capul unei liste
    Ex: consList '(2 3) '(1 2) == '((2 3) 1 2)
  - headSList, ia primul element dintr-un SList
  - tailSList, ia restul SList-ului
  - deepEqual, o funcție ce verifică egalitatea a două SList
  - flatten, întoarce lista cu elementele din SList (pe același nivel)
-}

data SList a = Atom a | List [SList a] deriving Show

emptySList :: SList a
emptySList = List []

consElem :: a -> SList a -> SList a
consElem x (Atom y)  = List $ [Atom x, Atom y]
consElem x (List xs) = List $ (Atom x : xs)

consList :: SList a -> SList a -> SList a
consList x (Atom y)  = List [x, Atom y]
consList x (List ys) = List $ x : ys

headSList :: SList a -> SList a
headSList (List x) = head x
headSList _        = error "head undefined on Atoms"

tailSList :: SList a -> SList a
tailSList (List x) = List $ tail x
tailSList _        = error "tail undefined on Atoms"

deepEqual :: Eq a => SList a -> SList a -> Bool
deepEqual (Atom x) (Atom y) = x == y
deepEqual (List a) (List b) = and $ zipWith deepEqual a b
deepEqual _ _               = False

flatten :: SList a -> [a]
flatten (Atom x)  = [x]
flatten (List xs) = concatMap flatten xs

check2 :: TestData
check2 = let l1 = consElem 1 $ emptySList
             l2 = consElem 2 $ consList (consElem 1 $ consElem 1 emptySList) $
                  consElem 3 emptySList
             l3 = consList (consElem 1 $ consElem 1 emptySList) $ consElem 3 $
                  emptySList
  in tests_ 2 $
          [ testCond "simple lists1" $ deepEqual l1 l1
          , testCond "simple lists 2 " $ not (deepEqual l1 l2)
          , testCond "less simple lists" $ deepEqual (consElem 2 $ l3) l2
          , testCond "head, tail" $ deepEqual (headSList $ tailSList l2) 
            (consElem 1 $ consElem 1 emptySList)
          , testVal "flatten" [2,1,1,3] $ flatten l2
          ]

{-
  3. Arbori binari de căutare
  Definiți un tip de date BST a pentru a implementa un arbore binar de
  căutare. De asemenea, definiți funcții pentru a insera o valoare într-un 
  arbore binar de căutare, căutarea unui element într-un arbore binar de 
  căutare dat, o funcție care întoarce lista elementelor din parcurgerea
  în inordine a arborelui și o funcție care întoarce cel mai mic subarbore
  care conține două noduri ale căror chei sunt date.
-}
data BST a = BSTNod a (BST a) (BST a) | BSTNil deriving Show

getElem :: (Ord a, Eq a) => BST a -> Maybe a
getElem BSTNil = Nothing
getElem (BSTNod elem _ _) = Just elem

insertElem :: (Ord a, Eq a) => BST a -> a -> BST a
insertElem BSTNil elem = BSTNod elem BSTNil BSTNil
insertElem root@(BSTNod value left right) elem
  | value == elem = root
  | value < elem = BSTNod value left (insertElem right elem)
  | value > elem = BSTNod value (insertElem left elem) right  

findElem :: (Ord a, Eq a) => BST a -> a -> Maybe a
findElem BSTNil _ = Nothing
findElem (BSTNod value left right) elem
  | value == elem = Just value
  | value < elem = findElem right elem
  | value > elem = findElem left elem

subTree :: (Ord a, Eq a) => BST a -> a -> a -> Maybe (BST a)
subTree BSTNil _ _ = Nothing
subTree root@(BSTNod value left right) value1 value2
  | (findElem left value1) == (Just value1) && (findElem left value2) == (Just value2) = subTree left value1 value2
  | (findElem right value1) == (Just value1) && (findElem right value2) == (Just value2) = subTree right value1 value2
  | (findElem root value1) == Nothing || (findElem root value2) == Nothing = Nothing
  | otherwise = Just root

inorder :: BST a -> [a]
inorder BSTNil = []
inorder (BSTNod elem left right) = (inorder left) ++ [elem] ++ (inorder right) 


check3 :: TestData
check3 = let root = foldl insertElem BSTNil [7, 4, 12, 2, 3, 1, 10, 15, 8]
             values = [1, 2, 3, 4, 7, 8, 10, 12, 15]
             tree1 = subTree root 1 3 
             tree2 = subTree root 4 5
             tree3 = subTree root 7 15
             tree4 = subTree root 1 8
  in tests_ 3 $ 
          [ testVal "findElem" (Just 3) $ findElem root 3
          , testVal "findElem" Nothing  $ findElem root 5
          , testSet "inorder" values    $ inorder root
          , testSet "subTree" [1, 2, 3] $ inorder (fromJust tree1)
          , testCond "subTree"          $ isNothing tree2
          , testSet "subTree" values    $ inorder (fromJust tree3)
          , testSet "subTree" values    $ inorder (fromJust tree4)
          ]

{-
  4. Arbore multicăi nevid
  Având dat tipul Tree a, definiți funcționala analoagă lui map, care să
  aplice o funcție asupra cheilor nodurilor din arbore, și o funcțională
  analoagă lui foldl care să parcurgă nodurile în ordinea: rădăcină, copil_1,
  copil_2, ... copil_n. 
-}

data Tree a = Node
  { value :: a
  , children :: [Tree a]
  } deriving (Eq, Show)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Node value children) = Node (f value) $ map (mapTree f) children

foldlTree :: (b -> a -> b) -> b -> Tree a -> b
foldlTree f acc (Node value children) = foldl (foldlTree f) (f acc value) children


check4 :: TestData
check4 = let tree = Node 1 [Node 2 [Node 3 []], Node 4 [Node 5 [], Node 6 [Node 7 [], Node 8 []]]]
  in tests_ 4 $
          [ testSet "foldlTree" [1..8] $ foldlTree (flip (:)) [] tree
          , testVal "foldlTree" 36     $ foldlTree (+) 0 tree
          , testVal "mapTree"   72     $ foldlTree (+) 0 $ mapTree (*2) tree
          ]

{-
  5. Difference lists
  Se cere definirea tipului de date "difference list".

  Un difference list este o listă "parțial construită", i.e. ale cărei
  elemente din coadă nu sunt (neapărat) în întregime cunoscute. De
  exemplu, ne putem imagina existența unei liste:

  1 : (2 : (3 : xs)) = [1,2,3] ++ xs

  unde xs nu are (la momentul construirii) o valoare cunoscută.

  În limbajele funcționale putem modela difference lists folosindu-ne de
  închideri: putem privi o astfel de structură ca pe o funcție care
  așteaptă un parametru (o listă) și întoarce o listă. Exemplul anterior
  poate fi astfel exprimat în funcție drept următoarea listă:

  (\ xs -> [1,2,3] ++ xs)

  Observație: Care este tipul lambda-ului de mai sus?

  Avantajul acestei abordări este că permite efectuarea oricărei
  operație de adăugare în listă (e.g. concatenarea cu o altă listă) în
  O(1), cu dezavantajul că eliminarea este în general mai puțin eficientă,
  deoarece presupune evaluarea elementelor structurii.

  Se cere, mai concret:
  - Definirea ADT-ului difference list (DList), „împăturit peste” o
    funcție de tipul [a] -> [a] (e.g. folosind construcția newtype)
  - Conversia [a] -> DL a (dlFromList) și invers (dlToList)
  - Lista vidă (emptyDL), adăugarea în capul unei liste (consDL) și în
    coada ei (snocDL)
  - Concatenarea a două liste (appendDL)
  - Operații de eliminare: primul element (headDL) și coada (tailDL)
    unei liste

  Operațiile de lucru cu difference lists (cu excepția celor de
  eliminare) vor fi implementate cât mai eficient posibil, i.e. fără a
  folosi dlFromList și dlToList.

  Pentru mai multe detalii, consultați link-ul:
  https://wiki.haskell.org/Difference_list
-}
newtype DList a = DL ([a] -> [a])

dlFromList :: [a] -> DList a
dlFromList xs = DL (xs ++)

dlToList :: DList a -> [a]
dlToList (DL f) = f []

emptyDL :: DList a
emptyDL = DL id

consDL :: a -> DList a -> DList a
consDL x (DL f) = DL $ (x:) . f

snocDL :: a -> DList a -> DList a
snocDL x (DL f) = DL $ f . (x:)

appendDL :: DList a -> DList a -> DList a
appendDL (DL f) (DL g) = DL $ f . g

headDL :: DList a -> Maybe a
headDL dl = case dlToList dl of
            []      -> Nothing
            (x : _) -> Just x

tailDL :: DList a -> Maybe (DList a)
tailDL dl = case dlToList dl of
            []       -> Nothing
            (_ : xs) -> Just $ dlFromList xs

check5 :: TestData
check5 = tests_ 5 $
          [ testVal "toList, fromList" "Ana are mere" $ dlToList (dlFromList "Ana are mere")
          , testVal "cons, empty" [1,2,3] $ dlToList $ consDL 1 $ consDL 2 $ consDL 3 emptyDL
          , testVal "snoc, empty" [3,2,1] $ dlToList $ snocDL 1 $ snocDL 2 $ snocDL 3 emptyDL
          , testVal "append" [1,2,3,4,5,6] $ dlToList $ dlFromList [1,2,3] `appendDL` dlFromList [4,5,6]
          , testCond "head, tail" $ case tailDL (dlFromList [1,2,3,4,5]) of
                                      Just dl -> case tailDL dl of
                                                 Just dl -> headDL dl == Just 3
          ]

{-
Helpers for testing :) You can also run check1, check2 etc independently
-}
check = quickCheck False [check1, check2, check3, check4, check5]
