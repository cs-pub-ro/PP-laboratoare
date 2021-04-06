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
reverseList l = undefined

-- pattern matching
reverseList2 :: [a] -> [a]
reverseList2 [] = []
reverseList2 (x : xl) = undefined

-- case of
reverseList3 :: [a] -> [a]
reverseList3 l = undefined

-- gărzi
reverseList4 :: [a] -> [a]
reverseList4 l = undefined

-- foldl
reverseList5 :: [a] -> [a]
reverseList5 l = undefined

-- foldr
reverseList6 :: [a] -> [a]
reverseList6 l = undefined

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
numToBase n b = undefined

-- pattern matching
numToBase2 :: Integer -> Integer -> [Integer]
numToBase2 0 _ = []
numToBase2 n b = undefined

-- gărzi
numToBase3 :: Integer -> Integer -> [Integer]
numToBase3 n b = undefined

-- case of
numToBase4 :: Integer -> Integer -> [Integer]
numToBase4 n b = undefined

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
removeDuplicatesLeft l = undefined

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
removeDuplicatesRight l = undefined

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
computeLength getLineSegment getStartPoint getEndPoint = undefined

-- cu where
computeLength2 :: ((Double, Double), (Double, Double)) 
            -> (((Double, Double), (Double, Double)) -> (Double, Double)) 
            -> (((Double, Double), (Double, Double)) -> (Double, Double)) -> Double
computeLength2 getLineSegment getStartPoint getEndPoint = undefined

-- Verificare: check5
check5 :: TestData
check5 = tests_ 5
    [ testVal "computeLength ((4, 2), (4, 5)) fst snd" 3.0 $ computeLength ((4, 2), (4, 5)) fst snd
    , testVal "computeLength ((2, 3), (6, 9)) fst snd" 7.211102550927978 $ computeLength ((2, 3), (6, 9)) fst snd
    , testVal "computeLength2 ((4, 2), (4, 5)) fst snd" 3.0 $ computeLength2 ((4, 2), (4, 5)) fst snd
    , testVal "computeLength2 ((2, 3), (6, 9)) fst snd" 7.211102550927978 $ computeLength2 ((2, 3), (6, 9)) fst snd
    ]

{-
    6. Să se găsească cuvintele care au lungimea cel puțin egală cu 10 caractere în două moduri:
        1) folosind filter
        2) folosind list comprehensions
-}

findStringsLongerThanTenChars :: [String] -> [String]
findStringsLongerThanTenChars l = undefined

findStringsLongerThanTenChars2 :: [String] -> [String]
findStringsLongerThanTenChars2 l = undefined

-- Verificare: check6
check6 :: TestData
check6 = tests_ 6
    [ testVal "findStringsLongerThanTenChars" ["programatoare","interesanta"] $
        findStringsLongerThanTenChars ["ana", "este", "o", "programatoare", "foarte", "interesanta"]
    , testVal "findStringsLongerThanTenChars2" ["programatoare","interesanta"] $
        findStringsLongerThanTenChars2 ["ana", "este", "o", "programatoare", "foarte", "interesanta"]
    ]

{-
    7. Să se construiască o listă de perechi de tip (string, lungime_string) în două moduri:
        1) folosind map
        2) folosind list comprehensions
-}

buildPairsStringLength :: [String] -> [(String, Int)]
buildPairsStringLength l = undefined

buildPairsStringLength2 :: [String] -> [(String, Int)]
buildPairsStringLength2 l = undefined

-- Verificare: check7
check7 :: TestData
check7 = tests_ 7
    [ testVal "buildPairsStringLength" [("ana",3),("este",4),("o",1), ("programatoare",13),("foarte",6),("interesanta",11)] $
        buildPairsStringLength ["ana", "este", "o", "programatoare", "foarte", "interesanta"]
    , testVal "buildPairsStringLength2" [("ana",3),("este",4),("o",1), ("programatoare",13),("foarte",6),("interesanta",11)] $
        buildPairsStringLength2 ["ana", "este", "o", "programatoare", "foarte", "interesanta"]
    ]

{-
    8. Implementați, folosind obligatoriu list-comprehensions, operații pe mulțimi:
    intersecție, diferență, produs cartezian. Utilizați ulterior funcțiile definite anterior
    pentru a reprezenta reuniunea mulțimilor.
-}

setIntersection :: Eq a => [a] -> [a] -> [a]
setIntersection a b = undefined

setDiff :: Eq a => [a] -> [a] -> [a]
setDiff a b = undefined

cartProduct :: [a] -> [b] -> [(a, b)]
cartProduct a b = undefined

setUnion :: Eq a => [a] -> [a] -> [a]
setUnion a b = undefined

-- Verificare: check8
check8 :: TestData
check8 = tests_ 8
    [ testSet "setIntersection" [1, 2, 6] $ setIntersection a b
    , testSet "setDiff" [3, 7] $ setDiff a b
    , testVal "cartProduct" [(1, 3), (1, 4), (1, 5), (2, 3), (2, 4), (2, 5)] $ cartProduct [1, 2] [3, 4, 5]
    , testSet "setUnion" [1, 2, 3, 4, 6, 7, 8, 10] $ setUnion a b
    ]
  where 
      a = [1, 7, 3, 6, 2]
      b = [2, 8, 6, 10, 4, 1]


{-
    9. Experimentați în consolă funcționalitățile funcției "trace", aplicată în funcțiile "evens", "squaredEvens" și "sumOfSquaredEvens".
-}
evens :: [Int]
evens = filter (\x -> trace ("even? " ++ show x) (even x)) [0..9]

squaredEvens :: [Int]
squaredEvens = map (\x -> trace ("square " ++ show x) x^2) evens

sumOfSquaredEvens :: Int
sumOfSquaredEvens = foldl' (\acc x -> trace ("add " ++ show x) (acc + x)) 0 squaredEvens

-- Verificare: check9
check9 :: TestData
check9 = tests_ 9
    [ testManually "To be experimented in ghci console" True ]

{-
    10. Scrieți o funcție "infiniteApply" care primește ca parametru o funcție "f"
    și o valoare inițială x0.
    
    Funcția ar trebui să întoarcă o listă infinită sub forma:
      x0, f x0, f (f x0), f (f (f x0)), ...
    
    Când testați funcția, fiți pregătiți să apăsați CTRL + C deoarece evaluarea
    aplicației va genera mult ouput.
 -}

infiniteApply :: (Double -> Double) -> Double -> [Double]
infiniteApply f x0 = undefined

-- Verificare: check10
check10 :: TestData
check10 = tests_ 10
    [ testVal "infiniteApply increment" [0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0] $ (take 10 $ infiniteApply (\x -> x + 1) 0)
    , testVal "infiniteApply powers of 2" [1.0,2.0,4.0,8.0,16.0,32.0,64.0,128.0,256.0,512.0, 1024.0,2048.0,4096.0,8192.0,16384.0,32768.0] $ (take 16 $ infiniteApply (\x -> x * 2) 1)
    ]


{-
    11. Avem o funcție "f" și derivata acesteia "df".

    Funcțiile "f" si "df" de mai jos sunt definite cu explicitarea parametrului,
    stil numit point-wise ("point" se referă la "x").
    
    Traduceți cele două definiții fără a mai explicita parametrul, utilizând
    compunerea de funcții, stil numit point-free. Astfel, definițiile ar trebui
    să fie de forma:
    f = ..., respectiv df = ..., fără a explicita x.

    (**) este operatorul de exponențiere pentru numere în virgulă mobilă.
 -}

f :: Double -> Double
-- f x = 36 - x ** 2 - de tradus această secvență în point-free
f x = undefined

df :: Double -> Double
-- df x = -2 * x - de tradus această secvență în point-free
df x = undefined

-- Verificare: check11
check11 :: TestData
check11 = tests_ 11
    [ testVal "f 6" 0 $ f 6
    , testVal "f 0" 36 $ f 0
    , testVal "df 6" (-12) $ df 6
    , testVal "df 0" 0 $ df 0
    ]

{-
    12. Pentru determinarea unei rădăcini a unei funcții, vom calcula aproximări
    repetate folosind metoda Newton-Raphson:
    
    x_new = x_old - f(x_old) / df(x_old)
    
    Inițial, x_old va fi o valoare aleatoare ("initial guess"), iar pe măsură
    ce vom aplica formula de mai sus in mod iterativ, rezultul va fi din ce în ce
    mai apropiat de soluția corectă (f(x) = 0).

    Dându-se o valoare inițială, calculați fluxul aproximărilor succesive
    obținute prin aplicarea formulei Newton-Raphson.
    Primul termen este considerat x_old.
    
    Hint:
    take + iterate (deja implementat in Haskell) / infinite_apply (implementat
    la exercițiul anterior)
 -}

newtonRaphson x g dg = undefined

-- Verificare: check11
check12 :: TestData
check12 = tests_ 12
    [ testVal "newtonRaphson 6 f df" [6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0] $ (take 10 $ newtonRaphson 6 f df) ]


{-
    13. Determinați soluția ecuației f(x) = 0.

    Deoarece metoda Netwon-Raphson folosește aproximări succesive pentru
    a ajunge la soluție și noi lucrăm in domeniul real, vom folosi o toleranță
    pentru a opri algoritmul.
    
    Pentru aceasta vom folosi doar toleranța absolută: |x_new - x_old| <= atol
    Pentru mai multe detalii despre toleranță (relativă vs absolută), consultați:
            http://web.mit.edu/10.001/Web/Tips/Converge.htm
    
    Hint 1: Pentru verificarea toleranței, puteți consulta pe rând diferențele
    dintre două aproximări succesive. În acest sens, puteți analiza fluxul 
    implementat la exercițiul anterior, în paralel cu o variantă decalată
    cu un element a acestuia, pentru a identifica prima diferență a cărei valoare
    absolută este mai mică decât toleranța:

          [t1,    t2,    t3,    t4,    ...] -
          [t0,    t1,    t2,    t3,    ...]
          ---------------------------------
          [t1-t0, t2-t1, t3-t2, t4-t3, ...]

    unde ti reprezinta al i-lea termen aflat cu metoda Newton-Raphson
    
    Hint 2: zip, dropWhile, abs
 -}

newtonRaphsonSolve :: Double -> Double -> (Double -> Double) -> (Double ->Double) -> Double
newtonRaphsonSolve x_old atol g dg = undefined

-- Verificare: check13
check13 :: TestData
check13 = tests_ 13
    [ testVal "newtonRaphsonSolve -3 0 f df" (-6) $ newtonRaphsonSolve (-3) 0 f df
    , testVal "newtonRaphsonSolve 3 0 f df" 6 $ newtonRaphsonSolve 3 0 f df
    ]


{-
    14. Metoda Babiloniană - folosită pentru aflarea rădăcinii pătrate a unui
    număr "a".
    
    Ca Newton-Raphson, și aceasta este o metodă iterativă cu pasul:
    x_new = 0.5 * (x_old + A / x_old), unde x_old va trebui inițializat
    cu o valoare aleatoare.
    
    Această metodă derivă din formula generală a lui Newton-Raphson
    pentru un f(x) specific:

    f(x) = x^2 - a
    df(x) = 2*x
    
    Implementați metoda Babilioniană
 -}

babylonianMethod :: Double -> Double -> Double -> Double
babylonianMethod a x_old atol = undefined

-- Verificare: check14
check14 :: TestData
check14 = tests_ 14
    [ testVal "babylonianMethod 36 3 0" 6 $ babylonianMethod 36 3 0
    , testVal "babylonianMethod 36 (-3) 0" (-6) $ babylonianMethod 36 (-3) 0
    ]
 
{-
Helpers for testing :) You can also run check1, check2 etc independently
-}
check = quickCheck False [check1, check2, check3, check4, check5, check6, check7, check8, check9, check10, check11, check12, check13, check14]