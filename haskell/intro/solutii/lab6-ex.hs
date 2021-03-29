{-# LANGUAGE NoMonomorphismRestriction #-}
import Data.List
import Debug.Trace
import TestPP
import Data.Char

{-
Expresia `undefined` are orice tip dar nu poate fi evaluată.
-}

{-
1. Traduceti codul Racket de mai jos in Haskell, in 4 moduri:
  1) cu if-then-else - reverseList
  2) cu pattern matching - reverseList2
  3) cu case of - reverseList3
  4) cu garzi - reverseList4

(define (rev L)
  (if (null? L)
      L
      (append (rev (cdr L))
              (list (car L)))))

De asemenea, implementati functia folosind foldl (reverseList5) si foldr (reverseList6)
-}

-- if-then-else
reverseList :: [a] -> [a]
reverseList l = if (null l) then [] else reverseList (tail l) ++ [(head l)]

-- pattern matching
reverseList2 :: [a] -> [a]
reverseList2 [] = []
reverseList2 (x : xl) = (reverseList2 xl) ++ [x]

-- case of
reverseList3 :: [a] -> [a]
reverseList3 l = case l of
    [] -> []
    (x : xl) -> (reverseList3 xl) ++ [x]

-- garzi
reverseList4 :: [a] -> [a]
reverseList4 l
    | null l = []
    | otherwise = reverseList4 (tail l) ++ [(head l)]

-- foldl
reverseList5 :: [a] -> [a]
reverseList5 l = foldl (\acc x -> x : acc) [] l

-- foldr
reverseList6 :: [a] -> [a]
reverseList6 l = foldr (\x acc -> acc ++ [x]) [] l

-- Verificare: check1
check1 :: TestPP ()
check1 = do
  assertVal "[1] reverseList" $
    reverseList [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] == [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
  assertVal "[1] reverseList2" $
    reverseList2 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] == [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
  assertVal "[1] reverseList3" $
    reverseList3 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] == [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
  assertVal "[1] reverseList4" $
    reverseList4 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] == [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
  assertVal "[1] reverseList5" $
    reverseList5 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] == [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
  assertVal "[1] reverseList6" $
    reverseList6 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] == [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]

{-
2. Traduceti codul Racket de mai jos in Haskell, in 4 moduri:
  1) cu if-then-else - numToBase
  2) cu pattern matching - numToBase2
  3) cu garzi - numToBase3
  4) cu case of - numToBase4

(define (num->base n b)
  (if (zero? n)
      '()
      (append (num->base (quotient n b) b)
              (list (modulo n b)))))
- pentru quotient folositi functia quot (exemplu: quot 7 2)
- pentru modulo folositi functia mod (exemplu: mod 7 2)
-}

-- if-then-else
numToBase :: Integer -> Integer -> [Integer]
numToBase n b = if (n == 0) then [] else (numToBase (quot n b) b) ++ [(mod n b)]

-- pattern matching
numToBase2 :: Integer -> Integer -> [Integer]
numToBase2 0 _ = []
numToBase2 n b = (numToBase2 (quot n b) b) ++ [(mod n b)]

-- garzi
numToBase3 :: Integer -> Integer -> [Integer]
numToBase3 n b 
    | n == 0 = []
    | otherwise = (numToBase3 (quot n b) b) ++ [(mod n b)]

-- case of
numToBase4 :: Integer -> Integer -> [Integer]
numToBase4 n b = case n of 
    0 -> []
    _ -> (numToBase4 (quot n b) b) ++ [(mod n b)]

-- Verificare: check2
check2 :: TestPP ()
check2 = do
  assertVal "[2] numToBase 489 2" $
    numToBase 489 2 == [1,1,1,1,0,1,0,0,1]
  assertVal "[2] numToBase 489 10" $
    numToBase 489 10 == [4,8,9]
  assertVal "[2] numToBase2 489 2" $
    numToBase2 489 2 == [1,1,1,1,0,1,0,0,1]
  assertVal "[2] numToBase2 489 10" $
    numToBase2 489 10 == [4,8,9]
  assertVal "[2] numToBase3 489 2" $
    numToBase3 489 2 == [1,1,1,1,0,1,0,0,1]
  assertVal "[2] numToBase3 489 10" $
    numToBase3 489 10 == [4,8,9]
  assertVal "[2] numToBase4 489 2" $
    numToBase4 489 2 == [1,1,1,1,0,1,0,0,1]
  assertVal "[2] numToBase4 489 10" $
    numToBase4 489 10 == [4,8,9]


{-
3. Traduceti codul Racket de mai jos in Haskell:

(define (remove-duplicates-left L)
  (reverse (foldl (λ (x acc)
                    (if (member x acc) acc
                        (cons x acc))) '() L)))
-}

removeDuplicatesLeft :: (Eq a) => [a] -> [a]
removeDuplicatesLeft l = reverse (foldl (\acc x -> if (elem x acc) then acc else (x : acc)) [] l)

-- Verificare: check3
check3 :: TestPP ()
check3 = do
  assertVal "[3] removeDuplicatesLeft [1, 2, 1, 3, 1, 4, 1, 5]" $
    removeDuplicatesLeft [1, 2, 1, 3, 1, 4, 1, 5] == [1,2,3,4,5]
  assertVal "[3] removeDuplicatesLeft [1, 2, 2, 3, 1, 3, 4, 5, 4, 5, 6]" $
    removeDuplicatesLeft [1, 2, 2, 3, 1, 3, 4, 5, 4, 5, 6] == [1,2,3,4,5,6]

{-
4. Traduceti codul Racket de mai jos in Haskell:

  (define (remove-duplicates-right L)
    (foldr (λ (x acc)
            (if (member x acc) acc
                (cons x acc))) '() L))
-}

removeDuplicatesRight :: (Eq a) => [a] -> [a]
removeDuplicatesRight l = foldr (\x acc -> if (elem x acc) then acc else (x : acc)) [] l

-- Verificare: check4
check4 :: TestPP ()
check4 = do
  assertVal "[3] removeDuplicatesRight [1, 2, 1, 3, 1, 4, 1, 5]" $
    removeDuplicatesRight [1, 2, 1, 3, 1, 4, 1, 5] == [2,3,4,1,5]
  assertVal "[3] removeDuplicatesRight [1, 2, 2, 3, 1, 3, 4, 5, 4, 5, 6]" $
    removeDuplicatesRight [1, 2, 2, 3, 1, 3, 4, 5, 4, 5, 6] == [2,1,3,4,5,6]

{-
Helpers for testing :)
-}
runAllTests = runTestPP $
  sequence_[check1, check2, check3, check4]
