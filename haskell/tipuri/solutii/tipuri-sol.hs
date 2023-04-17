{-
  PP, laboratorul 8: Tipuri de date utilizator
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
  în inordine a arborelui.
  Recomandăm utilizarea alias-urilor (@) pentru rezolvarea acestui exercițiu
  într-o manieră cât mai elegantă.

  De asemenea, definiți funcțiile de size și de height pentru determinarea
  numărului de noduri din arbore, respectiv înălțimii arborelui.

  Un arbore binar de căutare este eficient atâta timp cât înălțimea lui
  este minimă. Cu alte cuvinte, crearea unui BST printr-o listă sortată
  crescător / descrescător este worst-case-scenario pentru un arbore și este
  echivalentul unei liste înlănțuite. Tot în cadrul laboratorului, implementați
  funcția isBalanced care verifică dacă arborele este balansat. Un arbore
  este balansat dacă diferența înălțimilor subarborilor unui nod este <= 1.
-}

data BST a = BSTNod
    { vl :: a
    , lt :: BST a
    , rt :: BST a
    } | BSTNil deriving Show

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
inorder (BSTNod elem left right) = inorder left ++ [elem] ++ inorder right

size :: BST a -> Int
size BSTNil = 0
size (BSTNod _ left right) = 1 + size left + size right

height :: BST a -> Int
height BSTNil = 0
height (BSTNod elem left right) = 1 + max (height left) (height right)

isBalanced :: (Ord a, Num a) => BST a -> Bool
isBalanced BSTNil = True
isBalanced  (BSTNod e left right)
    | not (isBalanced left) = False
    | not (isBalanced right) = False
    | abs (height left - height right) > 1 = False
    | otherwise = True

check2 :: TestData
check2 = let root = foldl insertElem BSTNil [7, 4, 12, 2, 3, 1, 10, 15, 8]
             values = [1, 2, 3, 4, 7, 8, 10, 12, 15]
             notBalanced = foldl insertElem BSTNil [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
             balanced = foldl insertElem BSTNil [5, 3, 7, 1, 4, 6, 8]
  in tests_ 2
          [ testVal "findElem" (Just 3) $ findElem root 3
          , testVal "findElem" Nothing  $ findElem root 5
          , testSet "inorder" values    $ inorder root
          , testVal "size" 9 $ size root
          , testVal "size" 10 $ size notBalanced
          , testVal "height" 4 $ height root
          , testVal "height" 10 $ height notBalanced
          , testCond "isBalanced" $ (not . isBalanced) notBalanced
          , testCond "isBalanced" $ isBalanced balanced
          ]

{-
 3. Cum ați reprezenta un arbore oarecare?

 Exercițiu testat manual de asistent
-}

data Tree a = TreeNode
  { val      :: a
  , children :: [Tree a]
  } deriving (Eq, Show)

check3 :: TestData
check3 = tests_ 3 [testManually "General Tree" True]

{-
 4. Liste imbricate

  Definiți un tip de date SList a care să aibă funcționalități
  asemănătoare listelor din limbajele Lisp (e.g. Scheme, Racket, Clojure),
  permițând componente la diferite niveluri de imbricare.
  Obs: Gândiți-vă la discuția despre nested lists de la curs când vă apucați
  de implementare! Definiți-vă constructorii esențiali pentru încapsularea
  listelor: Atom - constructorul de date pentru un element
            List - constructorul de date pentru liste de NestedList
  Obs: Țineți cont de ambii constructori în implementarea operațiilor pe liste!
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

data NestedList a = Atom a | List [NestedList a] deriving Show

emptyList :: NestedList a
emptyList = List []

consElem :: a -> NestedList a -> NestedList a
consElem x (Atom y)  = List [Atom x, Atom y]
consElem x (List xs) = List (Atom x : xs)

consList :: NestedList a -> NestedList a -> NestedList a
consList x (Atom y)  = List [x, Atom y]
consList x (List ys) = List $ x : ys

headList :: NestedList a -> NestedList a
headList (List x) = head x
headList _        = error "head undefined on Atoms"

tailList :: NestedList a -> NestedList a
tailList (List x) = List $ tail x
tailList _        = error "tail undefined on Atoms"

deepEqual :: Eq a => NestedList a -> NestedList a -> Bool
deepEqual (Atom x) (Atom y) = x == y
deepEqual (List a) (List b) = and (zipWith deepEqual a b) && sameLength
  where
    sameLength = length a == length b
deepEqual _ _               = False

flatten :: NestedList a -> [a]
flatten (Atom x)  = [x]
flatten (List xs) = concatMap flatten xs

check4 :: TestData
check4 = let l1 = consElem 1 emptyList
             l2 = consElem 2 $ consList (consElem 1 $ consElem 1 emptyList) $
                  consElem 3 emptyList
             l3 = consList (consElem 1 $ consElem 1 emptyList) $ consElem 3 emptyList
             l4 = consElem 1 $ consElem 1 emptyList
  in tests_ 4
          [ testCond "simple lists1" $ deepEqual l1 l1
          , testCond "simple lists2" $ not (deepEqual l1 l2)
          , testCond "less simple lists" $ deepEqual (consElem 2 l3) l2
          , testCond "head, tail" $ deepEqual (headList $ tailList l2)
            (consElem 1 $ consElem 1 emptyList)
          , testCond "deepEqual length" $ not (deepEqual l1 l4)
          , testVal "flatten" [2,1,1,3] $ flatten l2
          ]

{-
 BONUS. Structuri infinite - Arbore binar

  Aveți mai jos rezolvată și explicată o problemă deosebită de
  generare de arbori infiniți. Găsiți și pașii de rezolvare în
  înregistrarea de anul trecut aici:
  https://ctipub-my.sharepoint.com/:v:/g/personal/mihnea_muraru_upb_ro/EcYo_vC9AktDt0fmeIqcdtEBZLsqbf4X6buhe9Gru8ocIg

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

  Putem construi un arbore binar infinit având ca rădăcină un nod cu
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
      de noduri, care vor fi expandate într-o listă de noduri copil (stânga, dreapta)
    * extractPath - primește un nod și întoarce calea către rădacină, o listă de perechi
      de forma (valoare, funcție_aplicată)
    * path - primește 2 numere x0 si xf și întoarce calea de la x0 la xf o listă de perechi
      de forma (valoare, funcție_aplicată). Întoarce o listă vidă în cazul în care nu se
      poate obține xf cu ajutorul funcțiilor date.

  Dacă aveți pe nodul stâng și nodul drept doar funcții monoton crescatoare, cum puteți
  opri căutarea?

  Similar exercițiului 4 din laboratorul anterior experimentați în consolă funcționalitățile
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
extractPath node = case parent node of
    Just x  -> (value x, func node) : extractPath x
    Nothing -> []

stopCond :: (Num a, Ord a) => a -> a -> Bool
stopCond xf val = val > 4 * xf

path :: (Ord a, Num a, Show a) => a -> a -> [(a, String)]
path x0 xf = go nodes
  where
    nodes = bfs $ completeBinaryTree x0
    go (node : nodes)
        | value node == xf         = reverse $ extractPath node
        | stopCond xf $ value node = []
        | otherwise                = go nodes

checkBonus :: TestData
checkBonus = let bfsNodes = bfs $ completeBinaryTree 1
                 values = [1, 2, 4, 4, 7, 8, 13, 8, 13, 14]
  in tests_ 5
          [ testVal "Bonus bfs" values (value <$> take 10 bfsNodes)
          , testVal "Bonus path 1 8" [(1,"g"),(4,"f")] $ path 1 8
          , testVal "Bonus path 1 100" [(1,"g"),(4,"f"),(8,"g"),(25,"f"),(50,"f")] $ path 1 100
          , testVal "Bonus path 1 16" [(1,"g"),(4,"f"),(8,"f")] $ path 1 16
          , testVal "Bonus path 1 10" [] $ path 1 10
          ]


{-
 Helpers for testing :) You can also run check1, check2 etc independently
-}
check = quickCheck False [check1, check2, check3, check4]
