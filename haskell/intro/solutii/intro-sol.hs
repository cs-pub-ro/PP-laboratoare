{-# LANGUAGE NoMonomorphismRestriction #-}
import Data.List
import Debug.Trace
import TestPP
import Data.Char

{-
Expresia `undefined` are orice tip dar nu poate fi evaluată.
-}

{-
    1. Traduceți codul Racket de mai jos în Haskell, în 4 moduri:
      1) cu if-then-else - reverseList
      2) cu pattern matching - reverseList2
      3) cu case of - reverseList3
      4) cu gărzi - reverseList4

    (define (rev L)
      (if (null? L)
          L
          (append (rev (cdr L))
                  (list (car L)))))

    De asemenea, implementați funcția folosind foldl (reverseList5) și foldr (reverseList6)
-}

-- if-then-else
reverseList :: [a] -> [a]
reverseList l = if null l 
                    then [] 
                    else reverseList (tail l) ++ [head l]

-- pattern matching
reverseList2 :: [a] -> [a]
reverseList2 [] = []
reverseList2 (x : xl) = reverseList2 xl ++ [x]

-- case of
reverseList3 :: [a] -> [a]
reverseList3 l = case l of
    [] -> []
    (x : xl) -> reverseList3 xl ++ [x]

-- gărzi
reverseList4 :: [a] -> [a]
reverseList4 l
    | null l = []
    | otherwise = reverseList4 (tail l) ++ [head l]

-- foldl
reverseList5 :: [a] -> [a]
reverseList5 l = foldl (\acc x -> x : acc) [] l

-- foldr
reverseList6 :: [a] -> [a]
reverseList6 l = foldr (\x acc -> acc ++ [x]) [] l

-- Verificare: check1
check1 :: TestData
check1 = tests_ 1
    [ testVal "reverseList" [10, 9, 8, 7, 6, 5, 4, 3, 2, 1] $ reverseList [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    , testVal "reverseList2" [10, 9, 8, 7, 6, 5, 4, 3, 2, 1] $ reverseList2 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    , testVal "reverseList3" [10, 9, 8, 7, 6, 5, 4, 3, 2, 1] $ reverseList3 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    , testVal "reverseList4" [10, 9, 8, 7, 6, 5, 4, 3, 2, 1] $ reverseList4 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    , testVal "reverseList5" [10, 9, 8, 7, 6, 5, 4, 3, 2, 1] $ reverseList5 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    , testVal "reverseList6" [10, 9, 8, 7, 6, 5, 4, 3, 2, 1] $ reverseList6 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    ]

{-
    2. Traduceți codul Racket de mai jos în Haskell, în 4 moduri:
      1) cu if-then-else - numToBase
      2) cu pattern matching - numToBase2
      3) cu gărzi - numToBase3
      4) cu case of - numToBase4

    (define (num->base n b)
      (if (zero? n)
          '()
          (append (num->base (quotient n b) b)
                  (list (modulo n b)))))
    - pentru quotient folosiți functia quot (exemplu: quot 7 2)
    - pentru modulo folosiți functia mod (exemplu: mod 7 2)
-}

-- if-then-else
numToBase :: Integer -> Integer -> [Integer]
numToBase n b = if n == 0 
                    then [] 
                    else numToBase (quot n b) b ++ [mod n b]

-- pattern matching
numToBase2 :: Integer -> Integer -> [Integer]
numToBase2 0 _ = []
numToBase2 n b = numToBase2 (quot n b) b ++ [mod n b]

-- gărzi
numToBase3 :: Integer -> Integer -> [Integer]
numToBase3 n b 
    | n == 0 = []
    | otherwise = numToBase3 (quot n b) b ++ [mod n b]

-- case of
numToBase4 :: Integer -> Integer -> [Integer]
numToBase4 n b = case n of 
    0 -> []
    _ -> numToBase4 (quot n b) b ++ [mod n b]

-- Verificare: check2
check2 :: TestData
check2 = tests_ 2
    [ testVal "numToBase 489 2" [1,1,1,1,0,1,0,0,1] $ numToBase 489 2
    , testVal "numToBase 489 10" [4,8,9] $ numToBase 489 10
    , testVal "numToBase2 489 2" [1,1,1,1,0,1,0,0,1] $ numToBase2 489 2
    , testVal "numToBase2 489 10" [4,8,9] $ numToBase2 489 10
    , testVal "numToBase3 489 2" [1,1,1,1,0,1,0,0,1] $ numToBase3 489 2
    , testVal "numToBase3 489 10" [4,8,9] $ numToBase3 489 10
    , testVal "numToBase4 489 2" [1,1,1,1,0,1,0,0,1] $ numToBase4 489 2
    , testVal "numToBase4 489 10" [4,8,9] $ numToBase4 489 10
    ]


{-
    3. Traduceți codul Racket de mai jos în Haskell:

    (define (remove-duplicates-left L)
      (reverse (foldl (λ (x acc)
                        (if (member x acc) acc
                            (cons x acc))) '() L)))
-}

removeDuplicatesLeft :: (Eq a) => [a] -> [a]
removeDuplicatesLeft l = reverse (foldl (\acc x -> if elem x acc then acc else (x : acc)) [] l)

-- Verificare: check3
check3 :: TestData
check3 = tests_ 3
    [ testVal "removeDuplicatesLeft [1, 2, 1, 3, 1, 4, 1, 5]" [1,2,3,4,5] $ removeDuplicatesLeft [1, 2, 1, 3, 1, 4, 1, 5]
    , testVal "removeDuplicatesLeft [1, 2, 2, 3, 1, 3, 4, 5, 4, 5, 6]" [1,2,3,4,5,6] $ removeDuplicatesLeft [1, 2, 2, 3, 1, 3, 4, 5, 4, 5, 6]
    ]

{-
    4. Traduceți codul Racket de mai jos în Haskell:

      (define (remove-duplicates-right L)
        (foldr (λ (x acc)
                (if (member x acc) acc
                    (cons x acc))) '() L))
-}

removeDuplicatesRight :: (Eq a) => [a] -> [a]
removeDuplicatesRight l = foldr (\x acc -> if elem x acc then acc else (x : acc)) [] l

-- Verificare: check4
check4 :: TestData
check4 = tests_ 4
    [ testVal "removeDuplicatesRight [1, 2, 1, 3, 1, 4, 1, 5]" [2,3,4,1,5] $ removeDuplicatesRight [1, 2, 1, 3, 1, 4, 1, 5]
    , testVal "removeDuplicatesRight [1, 2, 2, 3, 1, 3, 4, 5, 4, 5, 6]" [2,1,3,4,5,6] $ removeDuplicatesRight [1, 2, 2, 3, 1, 3, 4, 5, 4, 5, 6]
    ]

{-
    5. Traduceți codul Racket de mai jos în Haskell în două moduri:
      1) folosind let - computeLength
      2) folosind where - computeLength2

      (define (compute-length get-line-segment get-start-point get-end-point)
        (let* ((segment get-line-segment)
              (start (get-start-point segment))
              (stop (get-end-point segment))
              (x1 (car start)) (y1 (cdr start))
              (x2 (car stop)) (y2 (cdr stop)))
          (sqrt (+ (sqr (- x1 x2)) (sqr (- y1 y2))))))

    - pentru putere folosiți (**) - exemplu: 4 ** 2
    - pentru radical folosiți sqrt - exemplu: sqrt 4
-}

-- cu let
computeLength :: ((Double, Double), (Double, Double)) 
            -> (((Double, Double), (Double, Double)) -> (Double, Double)) 
            -> (((Double, Double), (Double, Double)) -> (Double, Double)) -> Double
computeLength getLineSegment getStartPoint getEndPoint = 
    let segment = getLineSegment
        start = getStartPoint segment
        end = getEndPoint segment
        startX = fst start
        startY = snd start
        endX = fst end
        endY = snd end
    in sqrt (((endY - startY) ** 2) + ((endX - startX) ** 2))

-- cu where
computeLength2 :: ((Double, Double), (Double, Double)) 
            -> (((Double, Double), (Double, Double)) -> (Double, Double)) 
            -> (((Double, Double), (Double, Double)) -> (Double, Double)) -> Double
computeLength2 getLineSegment getStartPoint getEndPoint = sqrt (((endY - startY) ** 2) + ((endX - startX) ** 2))
    where
        segment = getLineSegment
        start = getStartPoint segment
        end = getEndPoint segment
        startX = fst start
        startY = snd start
        endX = fst end
        endY = snd end

-- Verificare: check5
check5 :: TestData
check5 = tests_ 5
    [ testVal "computeLength ((4, 2), (4, 5)) fst snd" 3.0 $ computeLength ((4, 2), (4, 5)) fst snd
    , testVal "computeLength ((2, 3), (6, 9)) fst snd" 7.211102550927978 $ computeLength ((2, 3), (6, 9)) fst snd
    , testVal "computeLength2 ((4, 2), (4, 5)) fst snd" 3.0 $ computeLength2 ((4, 2), (4, 5)) fst snd
    , testVal "computeLength2 ((2, 3), (6, 9)) fst snd" 7.211102550927978 $ computeLength2 ((2, 3), (6, 9)) fst snd
    ]

{-
    6. Traduceți codul Racket de mai jos în Haskell în două moduri:
      1) folosind let - mergeSort
      2) folosind where - mergeSort2

    (define (merge L1 L2)
        (cond
            [(null? L1) L2]
            [(null? L2) L1]
            [(<= (car L1) (car L2)) (cons (car L1) (merge (cdr L1) L2))]
            [else (cons (car L2) (merge L1 (cdr L2)))]))

    (define (merge-sort L)
        (let*
            ((compute-half (λ (f lst) (f lst (quotient (length lst) 2))))
            (fst-half (λ (lst) (compute-half take lst)))
            (snd-half (λ (lst) (compute-half drop lst))))
        (cond
            [(null? L) '()]
            [(null? (cdr L)) L]
            [else (merge (merge-sort (fst-half L)) (merge-sort (snd-half L)))])))
-}

merge :: [Int] -> [Int] -> [Int]
merge [] l = l
merge l [] = l
merge (x1:l1) (x2:l2) = if x1 <= x2 then x1 : (merge l1 (x2:l2)) else x2 : (merge (x1:l1) l2)

mergeSort :: [Int] -> [Int]
mergeSort lst = 
    let computeHalf f l = f (quot (length l) 2) l
        fstHalf l = computeHalf take l
        sndHalf l = computeHalf drop l
    in
        case lst of
            [] -> []
            [x] -> lst
            _ -> merge (mergeSort (fstHalf lst)) (mergeSort (sndHalf lst))

mergeSort2 :: [Int] -> [Int]
mergeSort2 lst = case lst of
    [] -> []
    [x] -> lst
    _ -> merge (mergeSort (fstHalf lst)) (mergeSort (sndHalf lst))
    where
        computeHalf f l = f (quot (length l) 2) l
        fstHalf l = computeHalf take l
        sndHalf l = computeHalf drop l

check6 :: TestData
check6 = tests_ 6
    [ testVal "mergeSort [1, 5, 2, 0, 3, 5, 10, 7, -1]" [-1,0,1,2,3,5,5,7,10] $ mergeSort [1, 5, 2, 0, 3, 5, 10, 7, -1]
    , testVal "mergeSort []" [] $ mergeSort []
    , testVal "mergeSort [1]" [1] $ mergeSort [1]
    , testVal "mergeSort2 [1, 5, 2, 0, 3, 5, 10, 7, -1]" [-1,0,1,2,3,5,5,7,10] $ mergeSort2 [1, 5, 2, 0, 3, 5, 10, 7, -1]
    , testVal "mergeSort2 []" [] $ mergeSort2 []
    , testVal "mergeSort2 [1]" [1] $ mergeSort2 [1]
    ]

{-
    7. Traduceți codul Racket de mai jos în Haskell în două moduri:
      1) folosind let - playerWins
      2) folosind where - playerWins2

    (define (player-wins? candies)
        (letrec
            ([player (lambda (candies)
                        (and (> candies 0)
                            (or (not (opponent (- candies 1)))
                                (not (opponent (- candies 2))))))]
            [opponent (lambda (candies)
                        (and (> candies 1)
                                (or (not (player (- candies 2)))
                                    (not (player (- candies 3))))))])
            (player candies)))

-}

playerWins :: Int -> Bool
playerWins candies = 
    let player candies = (candies > 0) && ((not (opponent (candies - 1))) || (not (opponent (candies - 2))))
        opponent candies = (candies > 1) && ((not (player (candies - 2))) || (not (player (candies - 3))))
    in
        player candies

playerWins2 :: Int -> Bool
playerWins2 candies = player candies
    where 
        player candies = (candies > 0) && ((not (opponent (candies - 1))) || (not (opponent (candies - 2))))
        opponent candies = (candies > 1) && ((not (player (candies - 2))) || (not (player (candies - 3))))

check7 :: TestData
check7 = tests_ 7
    [ testVal "playerWins 2" True $ playerWins 2
    , testVal "playerWins 4" False $ playerWins 4
    , testVal "playerWins 17" True $ playerWins 17
    , testVal "playerWins 32" False $ playerWins 32
    , testVal "playerWins2 2" True $ playerWins2 2
    , testVal "playerWins2 4" False $ playerWins2 4
    , testVal "playerWins2 17" True $ playerWins2 17
    , testVal "playerWins2 32" False $ playerWins2 32
    ]

{-
Helpers for testing :) You can also run check1, check2 etc independently
-}
check = quickCheck False [check1, check2, check3, check4, check5, check6, check7]