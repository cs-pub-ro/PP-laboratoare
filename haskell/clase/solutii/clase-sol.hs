{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Maybe
import Data.List (sort, insertBy)
import Data.Function (on)
import TestPP


 ----------------------------------------------------------------------------------------------------
{- Setup Testare -}

valsInt = [20, 100, 30, 500, 1000, 30023, 513]
valsStr = ["PP", "PA", "PC", "AA", "LFA", "IA", "ML"]
prios = [5, 2, 10, -3, 1, 20]
elemsInt = zip prios valsInt
elemsStr = zip prios valsStr

{- 
    1. Ne dorim reprezentarea jocului X și 0 ca în formatul de mai jos.
    0X0
    X 0
    X0X
    Pentru fiecare stare trebuie reținute mutările fiecărui jucător,
    marcate cu X sau 0, iar pentru o căsuță liberă, spațiu.
    Pentru simplitate, checkerul va face mutările (nu trebuie
    reținut jucătorul care trebuie să mute în starea curentă).
-}

{-
    Vom reprezenta poziția sub forma unui tuplu.
-}
type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea celulelor de X și 0.
-}
data Cell = X | Zero | Blank 
    deriving Eq

{-
    Definiți instanța Show a tipului de date Cell pentru constructorii aleși.
-}

instance Show Cell
    where 
        show X = "X"
        show Zero = "0"
        show Blank = " "

{-
    Tip de date pentru reprezentarea stării curente a jocului.
    Conține o listă cu cele 9 celule ale jocului.
-}
data TicTacToe = Board [Cell]

{-
    Funcția trebuie să întoarcă un nivel gol - niciun
    jucător nu a făcut vreo mutare.
-}

emptyBoard :: TicTacToe
emptyBoard = Board $ replicate 9 Blank

{-
    Funcția primește tipul de celulă de inserat('X'/'0'), poziția la care să insereze și board-ul și 
    întoarce un board nou cu celula respectivă actualizată. Nu trebuie făcute alte verificări,
    pozițiile de test vor fi valide, iar celula de înlocuit va fi mereu goală.
    Poziția este dată sub formă de tuplu (linie, coloană), deci va trebui să calculați indexul 
    la care se află celula în listă.
-}

place :: Char -> Position -> TicTacToe -> TicTacToe
place playerCell pos@(x,y) (Board b) = Board $ take n b ++ [cell] ++ drop (n + 1) b
    where 
        n = x * 3 + y
        cell
            | playerCell == 'X' = X
            | otherwise = Zero

{-
    Completați instanța Show a jocului astfel încât să obțineți o hartă ca în exemplul
    de mai jos:
    0X0
    X 0
    X0X
    Liniile trebuie separate prin \n.
    Hints: 
        Folosiți-vă de instanța show a celulelor.
        Puteți construi board-ul linie cu linie.
-}
instance Show TicTacToe
    where 
        show (Board board) = first ++ "\n" ++ second ++ "\n" ++ third where
            first = concatMap show $ take 3 board
            second = concatMap show $ take 3 $ drop 3 board
            third = concatMap show $ drop 6 board
{-
    Definiți instanța Eq a stărilor jocului.
    Două stări sunt egale dacă lista de celule este aceeași.
-}
instance Eq TicTacToe
    where
        (Board b1) == (Board b2) = b1 == b2
{-
    Completați instanța Ord a stărilor jocului. O stare este 
    "mai mică" decât cealaltă dacă s-au efectuat mai puține mutări.
-}
instance Ord TicTacToe
    where 
        (Board b1) <= (Board b2) = moves b1 <= moves b2 where
            moves = length . filter (/= Blank)

-- Test 1

check1 :: TestData
check1 = let
        empty = "   \n   \n   "
        board = "X  \n 0 \n 0 "
    in tests_ 1 $
        [ testVal "showEmpty"  empty $ show emptyBoard
        , testVal "showBoard" board $ show $ place '0' (2,1) $ place 'X' (0,0) $ place '0' (1,1) emptyBoard
        , testVal "Eq 1" True $ emptyBoard == emptyBoard
        , testVal "Eq 2" False $ emptyBoard == place 'X' (0,0) emptyBoard
        , testVal "Ord 1" True $ place '0' (1,1) emptyBoard > emptyBoard
        , testVal "Ord 2" False $ emptyBoard > place 'X' (2,2) emptyBoard
        ]

{-
    Următoarele exerciții vor avea drept scop implementarea unei mici biblioteci
    pentru o coadă de priorități (mai specific, un max priority queue - "primul" element
    are prioritatea cea mai mare).
    Coada de priorități va fi reprezentată folosind o listă sau un arbore binar.
    Biblioteca noastră va defini o reprezentare generală pentru coada de priorități,
    precum și funcții care operează pe aceasta.
-}

-- Considerăm că un element din coadă este reprezentat de un tuplu care va conține:
-- * prioritatea
-- * valoarea
type Prio = Int

{-
    2. Analizați clasa PQueue definită mai jos și scrieți implementările
    implicite pentru funcțiile din această clasă:
    * fromList
    * toList

    Clasa PQueue definește interfața pentru toate structurile de coada de priorități
    pe care le vom implementa mai jos.
-}

class (Ord a) => PQueue pq a where

    -- Construiește o coadă de priorități goală
    empty :: pq a

    -- Verifică dacă coada este goală
    isEmpty :: pq a -> Bool

    -- Inserează elem in coada cu priorități
    insert :: (Prio, a) -> pq a -> pq a

    -- Întoarce primul element din coada de priorități
    top :: pq a -> Maybe (Prio, a)

    -- Șterge primul element din coada de priorități
    -- Dacă coada nu are elemente se va returna coada goală
    pop :: pq a -> pq a

    {-
        Creează o coadă de priorități dintr-o lista de tupluri
        Constrângeri: folosiți foldr!
    -}
    fromList :: [(Prio, a)] -> pq a
    fromList = foldr insert empty

    toList :: pq a -> [(Prio, a)]
    toList queue
        | isEmpty queue = []
        | otherwise = (fromJust $ top queue) : (toList $ pop queue)

    size :: pq a -> Int
    size = length . toList

-- Test 2
check2 :: TestData
check2 = tests_ 2 $ [testManually "fromList/toList" True]
-------------------------------------------------------------------------------

{-
    3. Definiți tipul ListPQ care reprezintă o coadă de priorități ca pe o
    listă de elemente. Includeți ListPQ în clasa PQueue.
-}

newtype ListPQ a = LPQ { lpq :: [(Prio, a)] }

instance (Ord a) => PQueue ListPQ a where
    empty = LPQ []

    insert node (LPQ queue) = LPQ $ insert' node queue
        where
            insert' node [] = [node]
            insert' node q@(x:xs)
                | (fst node) >= (fst x) = node:q
                | otherwise = x : insert' node xs

    top (LPQ []) = Nothing
    top (LPQ queue) = Just $ head queue

    pop (LPQ []) = empty
    pop (LPQ queue) = LPQ $ tail queue

    isEmpty (LPQ queue) = null queue

-- Test 3
listPQInt :: ListPQ Int
listPQInt = fromList elemsInt

listPQStr :: ListPQ String
listPQStr = fromList elemsStr

check3 :: TestData
check3 = tests_ 3 $
        [ testVal "ListPQ Int check" (reverse $ sort elemsInt) $ toList listPQInt
        , testVal "ListPQ Str check" (reverse $ sort elemsStr) $ toList listPQStr
        ]
{-
    LeftistPQ reprezintă o coadă de priorități ca pe un arbore binar.
    Fiecare nod va conține elementele:
    * Prioritatea
    * Rank-ul
    * Subarborele stang
    * Subarborele drept
    Referință - pentru mai multe detalii despre construcție: http://typeocaml.com/2015/03/12/heap-leftist-tree/
    Vizualizare: https://www.cs.usfca.edu/~galles/visualization/LeftistHeap.html
-}

type Rank = Int

data LeftistPQ a = Empty { rank :: Rank } |
                   Node { rank :: Rank, nodeVal :: (Prio, a), left :: LeftistPQ a, right :: LeftistPQ a }

{-
    4. Definiți operația de "merge" care primește 2 parametri de tipul LeftistPQ și intoarce
    un nou LeftistPQ obținut prin combinare.
    Cazuri de tratat:
    * Dacă unul dintre noduri este Empty
    * Dacă ambele noduri sunt Empty
    * Dacă nodurile nu sunt Empty
    Trebuie definită și operația inorder pentru parcurgerea arborelui - este folosit la validare
-}

merge :: LeftistPQ a -> LeftistPQ a -> LeftistPQ a

merge (Empty _) node = node
merge node (Empty _) = node
merge n1@(Node _ val1 lt1 rt1) n2@(Node _ val2 lt2 rt2)
    | (fst val1) >= (fst val2) = merge' val1 lt1 (merge rt1 n2)
    | otherwise = merge' val2 lt2 (merge n1 rt2)

merge' :: (Prio, a) -> LeftistPQ a -> LeftistPQ a -> LeftistPQ a

merge' val lt rt 
    | rank lt >= rank rt = Node (rank rt + 1) val lt rt
    | otherwise = Node (rank lt + 1) val rt lt

inorder :: LeftistPQ a -> [(Prio, a)]
inorder (Empty _) = []
inorder (Node _ val lt rt) = (inorder lt) ++ [val] ++ (inorder rt)

-- Test 4
check4 :: TestData
check4 = tests_ 4
          [
            testVal "Inorder Merge Empty NotEmpty" [(3,4)] $ inorder $ merge emptyNode node1,
            testVal "Inorder Merge NotEmpty Empty" [(3,4)] $ inorder $ merge node1 emptyNode,
            testVal "Inorder Merge NotEmpty NotEmpty" [(3,4), (5,10)] $ inorder $ merge node1 node2,
            testVal "Inorder Merge" [(3,4), (5,10), (10,20), (4,10)] $ inorder $ merge node4 $ merge node3 $ merge node1 node2
          ]
        where
          emptyNode = Empty 0
          node1 = Node 1 (3, 4) emptyNode emptyNode
          node2 = Node 1 (5, 10) emptyNode emptyNode
          node3 = Node 1 (10, 20) emptyNode emptyNode
          node4 = Node 1 (4, 10) emptyNode emptyNode

          {-
           - Inorder Merge Test:
           - Primul merge:
           -      (5,10)
           -       / \ 
           -      /   \
           -     /     \
           -  (3,4)   ---
           -
           -
           - Al doilea merge:
           -        (10,20)
           -         /  \
           -        /    \
           -       /      \
           -    (5,10)    ---
           -     /
           -    /
           -  (3,4)
           -
           -  Al treilea merge:
           -         (10,20)
           -           /  \
           -          /    \
           -         /      \
           -      (5,10)    (4,10)
           -       /
           -      /
           -     /
           -   (3,4)
           -}
{-
    5. Includeți LeftistPQ în PQueue
-}

instance (Ord a) => PQueue LeftistPQ a where

    empty = Empty 0

    isEmpty (Empty _) = True
    isEmpty _ = False

    insert val node = merge node $ Node 1 val empty empty

    top (Empty _) = Nothing
    top (Node _ val _ _) = Just val

    pop (Empty _) = empty
    pop (Node _ _ lt rt) = merge lt rt

-- Test 5
leftistPQInt :: LeftistPQ Int
leftistPQInt = fromList elemsInt

leftistPQStr :: LeftistPQ String
leftistPQStr = fromList elemsStr

check5 :: TestData
check5 = tests_ 5
          [
            testVal "LeftistPQ toList" (reverse $ sort elemsInt) $ toList leftistPQInt,
            testVal "LeftistPQ toList" (reverse $ sort elemsStr) $ toList leftistPQStr
          ]

{-
    6. Definiți funcția convert care face conversia intre cele 2 tipuri de reprezentări
-}

convert :: (PQueue pq1 a, PQueue pq2 a) => pq1 a -> pq2 a

convert = fromList . toList

-- Test 6
check6 :: TestData
check6 = tests_ 6
          [
            testVal "Convert ListPQInt to LeftistPQInt" (toList listPQInt) $ toList convertedLeftistPQInt,
            testVal "Convert ListPQStr to LeftistPQStr" (toList listPQStr) $ toList convertedLeftistPQStr,
            testVal "Convert LeftistPQInt to ListPQInt" (toList leftistPQInt) $ toList convertedListPQInt,
            testVal "Convert LeftistPQStr to ListPQStr" (toList leftistPQStr) $ toList convertedListPQStr
          ]
        where
          convertedLeftistPQInt :: LeftistPQ Int
          convertedLeftistPQInt = convert listPQInt

          convertedLeftistPQStr :: LeftistPQ String
          convertedLeftistPQStr = convert listPQStr

          convertedListPQInt :: ListPQ Int
          convertedListPQInt = convert leftistPQInt

          convertedListPQStr :: LeftistPQ String
          convertedListPQStr = convert leftistPQStr
{-
    7. Adăugați o nouă funcție "size" în clasa PQueue care întoarce numărul de elemente din coadă
    Atenție: Trebuie să fie definită implicit în PQueue
-}

-- Test 7
check7 :: TestData
check7 = tests_ 7
          [
            testVal "Size ListPQ Int" (size listPQInt) refSize,
            testVal "Size LeftistPQ Int" (size leftistPQInt) refSize
          ]
        where
          refSize = length elemsInt
{-
    8.(BONUS)  Adăugați tipurile ListPQ și LeftistPQ în clasa Foldable
        Funcția f primește drept parametri: o valoare din coadă (al doilea element din tuplu)
    și acumulatorul.
        Pentru ListPQ foldr ar trebui să aibă același comportament ca foldr.
        Pentru LeftistPQ foldr ar trebui să parcurgă arborele dreapta, rădăcină, stânga.


    Reminder:
        :t foldr
        foldr :: (a -> b -> b) -> b -> [a] -> b

        În Haskell 8.x.x tipul arată în felul următor
        :t foldr
        foldr :: (Foldable t) => (a -> b -> b) -> b -> t a -> b

        Clasa Foldable caracterizează tipurile care pot fi "reduse" la o anumită valoare utilizând operații specifice (foldr, foldl).
        Foldable t este o constrângere de tip, iar "t a" este un container care conține valori de tipul a.

        Mai multe informații: https://wiki.haskell.org/Foldable_and_Traversable
-}

instance Foldable ListPQ where
    foldr f acc = foldr (f . snd) acc . lpq

instance Foldable LeftistPQ where
    foldr f acc queue = foldr (f . snd) acc $ inorder queue

-- Test 8
check8 :: TestData
check8 = tests_ 8
          [
            testVal "Foldable ListPQ Int" 0 $ foldr fInt 0 listPQInt,
            testVal "Foldable ListPQ Str" "IAPCPPPALFAAA" $ foldr fStr "" listPQStr,
            testVal "Foldable LeftistPQ Int" 0 $ foldr fInt 0 leftistPQInt,
            testVal "Foldable LeftistPQ Str" "AAPCPAIALFAPP" $ foldr fStr "" leftistPQStr
          ]
        where
          fStr = (++)
          fInt = (*)

          {-
           -     Leftist PQ (Rank, Priority, Value)
           -           (2,        20,     "IA")
           -             /                    \
           -            /                      \
           -           /                        \
           -       (2, 10, "PC")                (1,    5,   "PP")
           -         /        \                     /         \
           -        /          \                   /           \
           -       /            \                 /             \
           -    (1, -3, "AA")   (1, 2, "PA")    (1, 1, "LFA")   ---
           -     /      \            /  \           /  \
           -    /        \          /    \         /    \
           -   /          \        /      \       /      \
           - ---          ---     ---     ---    ---     ---
           -
           -}


{-
    9.  Adăugați tipurile ListPQ și LeftistPQ în clasa Functor
    Funcția f primește ca parametru o valoare din coadă (al doilea element din tuplu)
    Folosiți constructorii LPQ și Node/Empty !
    
    class Functor f where
        fmap :: (a -> b) -> f a -> f b
-}

instance Functor ListPQ where
    fmap f (LPQ queue) = LPQ $ map (\ (x,y) -> (x, f y)) queue

instance Functor LeftistPQ where
    fmap f node = case node of
        Node r el left right -> Node r (fmap f el) (fmap f left) (fmap f right)
        _ -> Empty 0
-- Test 9
check9 :: TestData
check9 = tests_ 9 
          [
            testVal "Functor ListPQ Int" refInt $ toList $ fmap fInt listPQInt,
            testVal "Functor ListPQ Str" refStr $ toList $ fmap fStr listPQStr,
            testVal "Functor LeftistPQ Int" refInt $ toList $ fmap fInt leftistPQInt,
            testVal "Functor LeftistPQ Str" refStr $ toList $ fmap fStr leftistPQStr
          ]
        where
          fInt = (+ 100)
          fStr = (++ "42")

          refInt = reverse $ sort $ map (\ (x,y) -> (x, fInt y)) elemsInt
          refStr = reverse $ sort $ map (\ (x,y) -> (x, fStr y)) elemsStr
{-
    10. Adăugați LeftistPQ în clasa Show
    Va trebui ca arborele să fie afișat în modul următor:
    "--" x nivel în arbore {valoare din nod}
    Dacă nodul este Empty atunci se va afișa în loc de {valoare din nod} "empty"
    Ex: Node _ (3,4) {Node _ (4,5) Empty Empty} {Node _ (5,6) {Node _ (6,7) Empty Empty} Empty} -- nu ne interesează rankul
    --(3,4)
    ----(4,5)
    ------empty
    ------empty
    ----(5,6)
    ------(6,7)
    --------empty
    --------empty
    ------empty <-- și aici este newline la final

    Hint: Parcurgere preordine
-}

instance (Show a) => Show (LeftistPQ a) where
    show node = showAux node 1
        where 
            printPrefix n = take (2 * n) $ repeat '-'
            showAux (Node _ val left right) lvl =
                            (printPrefix lvl) ++ (show val) ++ "\n" ++ (showAux left (lvl + 1)) ++ (showAux right (lvl + 1) )
            showAux _ lvl = (printPrefix lvl) ++ "empty" ++ "\n"
-- Test 9
check10 :: TestData
check10 = tests_ 10
          [
            testVal "Show LeftistPQ Str" refLeftistPQStr $ show leftistPQStr,
            testVal "Show LeftistPQ Int" refLeftistPQInt $ show leftistPQInt
          ]
        where
          refLeftistPQStr = "--(20,\"IA\")\n\
                           \----(10,\"PC\")\n\
                           \------(-3,\"AA\")\n\
                           \--------empty\n\
                           \--------empty\n\
                           \------(2,\"PA\")\n\
                           \--------empty\n\
                           \--------empty\n\
                           \----(5,\"PP\")\n\
                           \------(1,\"LFA\")\n\
                           \--------empty\n\
                           \--------empty\n\
                           \------empty\n"

          refLeftistPQInt = "--(20,30023)\n\
                           \----(10,30)\n\
                           \------(-3,500)\n\
                           \--------empty\n\
                           \--------empty\n\
                           \------(2,100)\n\
                           \--------empty\n\
                           \--------empty\n\
                           \----(5,20)\n\
                           \------(1,1000)\n\
                           \--------empty\n\
                           \--------empty\n\
                           \------empty\n"

check = quickCheck False [check1, check2, check3, check4, check5, check6, check7, check8, check9, check10]