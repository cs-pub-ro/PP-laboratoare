{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Maybe
import Data.List (sort, insertBy)
import Data.Function (on)
import TestPP

{- 
    Ne dorim reprezentarea jocului X și 0 ca în formatul de mai jos.
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
    Definim Show a tipului de date Cell pentru constructorii aleși.
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
    Completăm instanța Show a jocului astfel încât să obținem o hartă ca în exemplul
    de mai jos:
    0X0
    X 0
    X0X
-}
instance Show TicTacToe
    where 
        show (Board board) = first ++ "\n" ++ second ++ "\n" ++ third where
            first = concatMap show $ take 3 board
            second = concatMap show $ take 3 $ drop 3 board
            third = concatMap show $ drop 6 board
{-
    Definim instanța Eq a stărilor jocului.
    Două stări sunt egale dacă lista de celule este aceeași.
-}
instance Eq TicTacToe
    where
        (Board b1) == (Board b2) = b1 == b2
{-
    Completăm instanța Ord a stărilor jocului. O stare este 
    "mai mică" decât cealaltă dacă s-au efectuat mai puține mutări.
-}
instance Ord TicTacToe
    where 
        (Board b1) <= (Board b2) = moves b1 <= moves b2 where
            moves = length . filter (/= Blank)

-- Test 1

check :: TestData
check = let
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