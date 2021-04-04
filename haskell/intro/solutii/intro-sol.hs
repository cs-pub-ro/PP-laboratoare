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

-- gărzi
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
numToBase n b = if (n == 0) then [] else (numToBase (quot n b) b) ++ [(mod n b)]

-- pattern matching
numToBase2 :: Integer -> Integer -> [Integer]
numToBase2 0 _ = []
numToBase2 n b = (numToBase2 (quot n b) b) ++ [(mod n b)]

-- gărzi
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
    3. Traduceți codul Racket de mai jos în Haskell:

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
    4. Traduceți codul Racket de mai jos în Haskell:

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
  assertVal "[4] removeDuplicatesRight [1, 2, 1, 3, 1, 4, 1, 5]" $
    removeDuplicatesRight [1, 2, 1, 3, 1, 4, 1, 5] == [2,3,4,1,5]
  assertVal "[4] removeDuplicatesRight [1, 2, 2, 3, 1, 3, 4, 5, 4, 5, 6]" $
    removeDuplicatesRight [1, 2, 2, 3, 1, 3, 4, 5, 4, 5, 6] == [2,1,3,4,5,6]


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
check5 :: TestPP ()
check5 = do
  assertVal "[5] computeLength ((4, 2), (4, 5)) fst snd" $
    computeLength ((4, 2), (4, 5)) fst snd == 3.0
  assertVal "[5] computeLength ((2, 3), (6, 9)) fst snd" $
    computeLength ((2, 3), (6, 9)) fst snd == 7.211102550927978
  assertVal "[5] computeLength2 ((4, 2), (4, 5)) fst snd" $
    computeLength2 ((4, 2), (4, 5)) fst snd == 3.0
  assertVal "[5] computeLength2 ((2, 3), (6, 9)) fst snd" $
    computeLength2 ((2, 3), (6, 9)) fst snd == 7.211102550927978

{-
    6. Să se găsească cuvintele care au lungimea cel puțin egală cu 10 caractere în două moduri:
        1) folosind filter
        2) folosind list comprehensions
-}

findStringsLongerThanTenChars :: [String] -> [String]
findStringsLongerThanTenChars l = filter (\x -> (length x) >= 10) l

findStringsLongerThanTenChars2 :: [String] -> [String]
findStringsLongerThanTenChars2 l = [x | x <- l, (length x) >= 10]

-- Verificare: check6
check6 :: TestPP ()
check6 = do
  assertVal "[6] findStringsLongerThanTenChars" $
    findStringsLongerThanTenChars ["ana", "este", "o", 
        "programatoare", "foarte", "interesanta"] == ["programatoare","interesanta"]
  assertVal "[6] findStringsLongerThanTenChars2" $
    findStringsLongerThanTenChars2 ["ana", "este", "o", 
        "programatoare", "foarte", "interesanta"] == ["programatoare","interesanta"]

{-
    7. Să se construiască o listă de perechi de tip (string, lungime_string) în două moduri:
        1) folosind map
        2) folosind list comprehensions
-}

buildPairsStringLength :: [String] -> [(String, Int)]
buildPairsStringLength l = map (\x -> (x, length x)) l

buildPairsStringLength2 :: [String] -> [(String, Int)]
buildPairsStringLength2 l = [(x, length x) | x <- l]

-- Verificare: check7
check7 :: TestPP ()
check7 = do
  assertVal "[7] buildPairsStringLength" $
    buildPairsStringLength ["ana", "este", "o", 
        "programatoare", "foarte", "interesanta"] == [("ana",3),("este",4),("o",1),
          ("programatoare",13),("foarte",6),("interesanta",11)]
  assertVal "[7] buildPairsStringLength2" $
    buildPairsStringLength2 ["ana", "este", "o", 
        "programatoare", "foarte", "interesanta"] == [("ana",3),("este",4),("o",1),
          ("programatoare",13),("foarte",6),("interesanta",11)]

{-
    8. Implementați, folosind obligatoriu list-comprehensions, operații pe mulțimi:
    intersecție, diferență, produs cartezian. Utilizați ulterior funcțiile definite anterior
    pentru a reprezenta reuniunea mulțimilor.
-}

setIntersection :: Eq a => [a] -> [a] -> [a]
setIntersection a b = [x | x <- a, x `elem` b]

setDiff :: Eq a => [a] -> [a] -> [a]
setDiff a b = [x | x <- a, x `notElem` b]

cartProduct :: [a] -> [b] -> [(a, b)]
cartProduct a b = [(x, y) | x <- a, y <- b]

setUnion :: Eq a => [a] -> [a] -> [a]
setUnion a b = a ++ setDiff b (setIntersection a b)

-- Verificare: check8
check8 :: TestPP ()
check8 = do
  let a = [1, 7, 3, 6, 2]
      b = [2, 8, 6, 10, 4, 1]
  assertVal "[8] setIntersection" $
    sort (setIntersection a b) == [1, 2, 6]
  assertVal "[8] setDiff" $
    sort (setDiff a b) == [3, 7]
  assertVal "[8] cartProduct" $
    cartProduct [1, 2] [3, 4, 5] == [(1, 3), (1, 4), (1, 5), (2, 3), (2, 4), (2, 5)]
  assertVal "[8] setUnion" $
    sort (setUnion a b) == [1, 2, 3, 4, 6, 7, 8, 10]


{-
    9. Experimentați în consolă funcționalitățile funcției "trace", aplicată în funcțiile "evens", "squaredEvens" și "sumOfSquaredEvens".
-}
evens :: [Int]
evens = filter (\x -> trace ("even? " ++ show x) (even x)) [0..9]

squaredEvens :: [Int]
squaredEvens = map (\x -> trace ("square " ++ show x) x^2) evens

sumOfSquaredEvens :: Int
sumOfSquaredEvens = foldl' (\acc x -> trace ("add " ++ show x) (acc + x)) 0 squaredEvens

{-
    10. Scrieți o funcție "infiniteApply" care primește ca parametru o funcție "f"
    și o valoare inițială x0.
    
    Funcția ar trebui să întoarcă o listă infinită sub forma:
      x0, f x0, f (f x0), f (f (f x0)), ...
    
    Când testați funcția, fiți pregătiți să apăsați CTRL + C deoarece evaluarea
    aplicației va genera mult ouput.
 -}

infiniteApply :: (Double -> Double) -> Double -> [Double]
infiniteApply f x0 = x0 : (infiniteApply f $ f x0)

-- Verificare: check10
check10 :: TestPP ()
check10 = do
  assertVal "[10] infiniteApply increment" $
    (take 10 $ infiniteApply (\x -> x + 1) 0) == [0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0]
  assertVal "[10] infiniteApply powers of 2" $
    (take 16 $ infiniteApply (\x -> x * 2) 1) == [1.0,2.0,4.0,8.0,16.0,32.0,64.0,128.0,256.0,512.0,
        1024.0,2048.0,4096.0,8192.0,16384.0,32768.0]


{-
    11. Având o funcție "f" și derivata acesteia "df", dorim să aflăm "x" știind că
    f(x) = 0. Cu alte cuvinte, "x" este o rădăcină a funcției "f".
    
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
f = (36-).(**2)

df :: Double -> Double
-- df x = -2 * x - de tradus această secvență în point-free
df = (*(-2))

-- Verificare: check11
check11 :: TestPP ()
check11 = do
  assertVal "[11] f 6" $
    f 6 == 0
  assertVal "[11] f 0" $
    f 0 == 36
  assertVal "[11] df 6" $
    df 6 == -12
  assertVal "[11] df 0" $
    df 0 == 0

{-
    12. Pentru determinarea unei rădăcini a unei funcții, vom calcula aproximări
    repetate folosind metoda Newton-Raphson:
    
    x_new = x_old - f(x_old) / df(x_old)
    
    Inițial, x_old va fi o valoare aleatoare ("initial guess"), iar pe măsură
    ce vom aplica formula de mai sus în mod iterativ, rezultul va fi din ce în ce
    mai apropiat de soluția corectă (f(x) = 0).

    Dându-se o valoare inițială, calculați fluxul aproximărilor succesive
    obținute prin aplicarea formulei Newton-Raphson.
    Primul termen este considerat x_old.
    
    Hint:
    take + iterate (deja implementat in Haskell) / infinite_apply (implementat
    la exercițiul anterior)

    Testați afișând primele 10 elemente ale acestui flux.
 -}

newtonRaphson x g dg = iterate (\x -> x - (g x) / (dg x)) x

-- Verificare: check11
check12 :: TestPP ()
check12 = do
  assertVal "[12] newtonRaphson 6 f df" $
    (take 10 $ newtonRaphson 6 f df) == [6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0]


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
    
    Rulați algoritmul pentru valorile inițiale: 3 si -3 (x_old = 3, x_old = -3)
 -}

newtonRaphsonSolve :: Double -> Double -> (Double -> Double) -> (Double ->Double) -> Double
newtonRaphsonSolve x_old atol g dg =
    snd $ head $ dropWhile (\(x, y) -> abs(x - y) > atol) vals
  where
    nr = newtonRaphson x_old g dg
    vals = zip (tail nr) nr

-- Verificare: check13
check13 :: TestPP ()
check13 = do
  assertVal "[13] newtonRaphsonSolve -3 0 f df" $
    newtonRaphsonSolve (-3) 0 f df == -6
  assertVal "[13] newtonRaphsonSolve 3 0 f df" $
    newtonRaphsonSolve 3 0 f df == 6


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
babylonianMethod a x_old atol = newtonRaphsonSolve x_old atol (\x->x**2-a) (\x->2*x)

-- Verificare: check14
check14 :: TestPP ()
check14 = do
  assertVal "[14] babylonianMethod 36 3 0" $
    babylonianMethod 36 3 0 == 6
  assertVal "[14] babylonianMethod 36 (-3) 0" $
    babylonianMethod 36 (-3) 0 == -6
 
{-
Helpers for testing :)
-}
runAllTests = runTestPP $
  sequence_[check1, check2, check3, check4, check5, check6, check7, 
            check8, check10, check11, check12, check13, check14]
