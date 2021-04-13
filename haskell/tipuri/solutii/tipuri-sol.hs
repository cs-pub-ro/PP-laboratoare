{-
  PP, laboratorul 7: tipuri de date utilizator
-}

import Data.List
import Data.Maybe
import Debug.Trace
import TestPP

{-
  1. Vectori
  Se dă tipul de date Vector, reprezentând vectori din spațiul R^3.

  Implementați produsul scalar (dot product) dintre doi vectori.

  Explicații

  Fie a și b doi vectori din R^3 considerați de forma:
  a = a1 * i + a2 * j + a3 * k
  b = b1 * i + b2 * j + b3 * k
  
  Produsul scalar al celor doi vectori o să fie egal cu:
  a • b = a1 * b1 + a2 * b2 + a3 * b3
  Produsul scalar a doi vectori u și v este 0 dacă și numai dacă u și v sunt ortogonali.

  Pentru mai multe detalii, consultați:
  https://gerardnico.com/linear_algebra/vector_vector
-}
data Vector = V
  { vx :: Double
  , vy :: Double
  , vz :: Double
  } deriving (Show, Eq)

dotV :: Vector -> Vector -> Double
dotV (V vx1 vy1 vz1) (V vx2 vy2 vz2) = vx1 * vx2 + vy1 * vy2 + vz1 * vz2

check1 :: TestData
check1 = let 
      v1 = V 1 (-1) 0
      v2 = V 1 1 0
  in test_ 1 $ testVal "dotV" 0.0 $ dotV v1 v2

{-
  2. Arbori binari de căutare

  Definiți un tip de date BST a pentru a implementa un arbore binar de
  căutare. Implementați funcții pentru a insera o valoare într-un 
  arbore binar de căutare, căutarea unui element într-un arbore binar de 
  căutare dat, o funcție care întoarce lista elementelor din parcurgerea
  în inordine a arborelui. De asemenea, definiți funcționala analoagă lui 
  map, care să aplice o funcție asupra cheilor nodurilor din arbore, și o 
  funcțională analoagă lui foldl care să parcurgă nodurile în ordinea: rădăcină, 
  nod_stanga, nod_dreapta ...

-}

data BST a = BSTNod a (BST a) (BST a) | BSTNil deriving Show

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

inorder :: BST a -> [a]
inorder BSTNil = []
inorder (BSTNod elem left right) = (inorder left) ++ [elem] ++ (inorder right) 

mapTree :: (a -> b) -> BST a -> BST b
mapTree f BSTNil = BSTNil
mapTree f (BSTNod value left right) = BSTNod (f value) (mapTree f left) $ mapTree f right

foldlTree :: (b -> a -> b) -> b -> BST a -> b
foldlTree _ acc BSTNil = acc
foldlTree f acc (BSTNod value left right) = foldlTree f newAcc right
  where newAcc = foldlTree f (f acc value) left 

check2 :: TestData
check2 = let root = foldl insertElem BSTNil [7, 4, 12, 2, 3, 1, 10, 15, 8]
             values = [1, 2, 3, 4, 7, 8, 10, 12, 15]
             foldValues = [15,8,10,12,3,1,2,4,7]
             mapValues = map (*2) values
             f = flip (:)
  in tests_ 2 $ 
          [ testVal "findElem" (Just 3) $ findElem root 3
          , testVal "findElem" Nothing  $ findElem root 5
          , testSet "inorder" values    $ inorder root
          , testVal "mapTree" mapValues $ inorder $ mapTree (*2) root
          , testVal "foldlTree (:)" foldValues $ foldlTree f [] root
          , testVal "foldl (+)" 62 $ foldlTree (+) 0 root 
          ]

{-
 3. Structuri infinite - Arbore binar 

  Pornind de la o valoare numerică x0, găsiți numărul minim de
  aplicări de funcții succesive f sau g necesare pentru a ajunge 
  la o valoare target xf.
  
  De exemplu:
    Fie f = \x -> 2 * x și g = \x -> 3 * x + 1
    * pentru a ajunge la valoarea 8 din x0 = 1 este nevoie de 2 aplicări:
      xf = 2 * 4 = f(4) = f(g(3 * 1 + 1)) = f(g(1)) = f(g(x0))
    * pentru a ajunge la valoarea 13 din x0 = 1 este nevoie de 2 aplicări:
      xf = 3 * 4 + 1 = g(4) = g(g(3 * 1 + 1)) = g(g(1)) = g(g(x0))
    * de la x0 = 1 la xf = 10 nu putem ajunge cu ajutorul funcțiilor anterior
      mentionate

  De ce ne ajută o structură arborescentă în acest caz?

  Putem construi un arbore binar infinit avand ca rădăcină un nod cu 
  valoarea x0. Pentru construirea nodului de pe ramura din stânga se 
  va aplica funcția f, iar pe ramura din dreapta se va aplica funcția g.

    Exemplu:
                               ┌─────┐
               ┌───────────────┤x0=1 ├────────────────┐
               │               └─────┘                │
               │                                      │
               │                                      │
           ┌───┴───┐                              ┌───┴───┐
         ┌─┤f(1)=2 ├───────────┐                 ┌┤g(1)=4 ├┐
         │ └───────┘           │                 │└───────┘│
         │                     │                 │         │
         │                     │                 │         │
     ┌───┴───┐             ┌───┴────┐        ┌───┴───┐ ┌───┴────┐
    ┌┤f(2)=4 ├┐           ┌┤g(2)=7  ├─┐      │f(4)=8 │ │g(4)=13 │
    │└───────┘│           │└────────┘ │      └───────┘ └────────┘
    │         │           │           │          │         │ 
    │         │           │           │        .....     .....
┌───┴───┐ ┌───┴────┐ ┌────┴────┐ ┌────┴────┐
│f(4)=8 │ │g(4)=13 │ │f(7)=14  │ │g(7)=22  │   
└───────┘ └────────┘ └─────────┘ └─────────┘
    │         │           │           │
  .....     .....       .....       .....

  Problema se transformă în găsirea unui drum minim între nodul radacină 
  și un nod dat.

  Astfel, extindeți tipul definit anterior (de exemplu puteți adăuga un 
  câmp părinte, un câmp string pentru a reține funcția aplicată pe nodul curent) 
  și implementați următoarele funcții:
    * completeBinaryTree - pornind de la x0 construiește arborele binar infinit aplicând
      f pe nodul stâng, respectiv g pe nodul drept
    * bfs - primește un arbore și întoarce parcurgerea bfs a acestuia - o lista infinită 
      de noduri, care vor fi expandate într-o listă de noduri copil (stanga, dreapta)
    * extractPath - primește un nod și întoarce calea către rădacină, o listă de perechi
      de forma (valoare, funcție_aplicată) 
    * path - primește 2 numere x0 si xf și întoarce calea de la x0 la xf o listă de perechi
      de forma (valoare, funcție_aplicată). Întoarce o listă vidă în cazul în care nu se 
      poate obține xf cu ajutorul funcțiilor date.

  Dacă aveți pe nodul stâng și nodul drept doar funcții monoton crescatoare, cum puteți
  opri căutarea?

  Similar exercițiului 9 din laboratorul anterior experimentați în consolă funcționalitățile 
  funcției "trace" în definirea nodurilor din funția completeBinaryTree. Ce observați?
  
-}

data InfBST a = Node
    { value   :: a
    , parent  :: Maybe (InfBST a)
    , func    :: String
    , left    :: InfBST a
    , right   :: InfBST a
    } deriving (Eq, Show)

f :: (Num a) => a -> a
f = \x -> 2 * x 

g :: (Num a) => a -> a
g = \x -> 3 * x + 1

completeBinaryTree :: (Show a, Num a) => a -> InfBST a
completeBinaryTree x0 = completeBinaryTreeHelper x0 Nothing ""

completeBinaryTreeHelper v p fStr = currentNode
  where
    currentNode = Node v p fStr leftNode rightNode
    leftNode  = trace ("| Node left " ++ show v ++ " -> f")  $ completeBinaryTreeHelper (f v) (Just currentNode) "f"
    rightNode = trace ("| Node right " ++ show v ++ " -> g") $ completeBinaryTreeHelper (g v) (Just currentNode) "g"

bfs :: (Num a) => InfBST a -> [InfBST a]
bfs root = nodes
  where
    nodes = root : children
    children = concatMap (\node -> [left node, right node]) nodes

extractPath :: (Num a, Show a) => InfBST a -> [(a, String)]

extractPath node = case (parent node) of 
    Just x  -> (value x, func node):(extractPath x)
    Nothing -> []

-- extractPath node = tail $ map (\(node, f) -> (value node, f)) nodes
--   where condition = not.isNothing.parent.fst
--         nodes = takeWhile condition $ iterate (\(node, f) -> (fromJust $ parent node, func node)) (node, "")

stopCond :: (Num a, Ord a) => a -> a -> Bool
stopCond xf val = val > 4 * xf 

path :: (Ord a, Num a, Show a) => a -> a -> [(a, String)]
path x0 xf = go nodes
  where
    nodes = bfs $ completeBinaryTree x0
    go (node:nodes)
              | value node == xf         = reverse $ extractPath node
              | stopCond xf $ value node = [] 
              | otherwise                = go nodes

check3 :: TestData
check3 = let bfsNodes = bfs $ completeBinaryTree 1
             values = [1, 2, 4, 4, 7, 8, 13, 8, 13, 14]
  in tests_ 3 $ 
          [ testVal "bfs" values (value <$> take 10 bfsNodes)
          , testVal "path 1 8" [(1,"g"),(4,"f")] $ path 1 8
          , testVal "path 1 100" [(1,"g"),(4,"f"),(8,"g"),(25,"f"),(50,"f")] $ path 1 100
          , testVal "path 1 16" [(1,"g"),(4,"f"),(8,"f")] $ path 1 16
          , testVal "path 1 10" [] $ path 1 10
          ]  
          
{-
 4. Cum ați reprezenta un arbore oarecare?
 
 Exercițiu testat manual de asistent
-}
data Tree a = TreeNode
  { val      :: a
  , children :: [Tree a]
  } deriving (Eq, Show)

check4 :: TestData
check4 = tests_ 4 $ [testManually "General Tree" True]

{-
 5. Liste imbricate

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

check5 :: TestData
check5 = let l1 = consElem 1 $ emptySList
             l2 = consElem 2 $ consList (consElem 1 $ consElem 1 emptySList) $
                  consElem 3 emptySList
             l3 = consList (consElem 1 $ consElem 1 emptySList) $ consElem 3 $
                  emptySList
  in tests_ 5 $
          [ testCond "simple lists1" $ deepEqual l1 l1
          , testCond "simple lists 2 " $ not (deepEqual l1 l2)
          , testCond "less simple lists" $ deepEqual (consElem 2 $ l3) l2
          , testCond "head, tail" $ deepEqual (headSList $ tailSList l2) 
            (consElem 1 $ consElem 1 emptySList)
          , testVal "flatten" [2,1,1,3] $ flatten l2
          ]

{-
  6. Difference lists
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

check6 :: TestData
check6 = tests_ 6 $
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
check = quickCheck False [check1, check2, check3, check4, check5, check6]
