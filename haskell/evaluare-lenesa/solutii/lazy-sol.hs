{-# LANGUAGE NoMonomorphismRestriction #-}
import Data.List
import Debug.Trace
import TestPP
import Data.Char

{-
Expresia `undefined` are orice tip dar nu poate fi evaluată.
-}

{-
    1. Să se găsească cuvintele care au lungimea cel puțin egală cu 10 caractere în două moduri:
        1) folosind filter
        2) folosind list comprehensions
-}

findStringsLongerThanTenChars :: [String] -> [String]
findStringsLongerThanTenChars l = filter (\x -> length x >= 10) l

findStringsLongerThanTenChars2 :: [String] -> [String]
findStringsLongerThanTenChars2 l = [x | x <- l, length x >= 10]

-- Verificare: check1
check1 :: TestData
check1 = tests_ 1
    [ testVal "findStringsLongerThanTenChars" ["programatoare","interesanta"] $
        findStringsLongerThanTenChars ["ana", "este", "o", "programatoare", "foarte", "interesanta"]
    , testVal "findStringsLongerThanTenChars2" ["programatoare","interesanta"] $
        findStringsLongerThanTenChars2 ["ana", "este", "o", "programatoare", "foarte", "interesanta"]
    ]

{-
    2. Să se construiască o listă de perechi de tip (string, lungime_string) în două moduri:
        1) folosind map
        2) folosind list comprehensions
-}

buildPairsStringLength :: [String] -> [(String, Int)]
buildPairsStringLength l = map (\x -> (x, length x)) l

buildPairsStringLength2 :: [String] -> [(String, Int)]
buildPairsStringLength2 l = [(x, length x) | x <- l]

-- Verificare: check2
check2 :: TestData
check2 = tests_ 2
    [ testVal "buildPairsStringLength" [("ana",3),("este",4),("o",1), ("programatoare",13),("foarte",6),("interesanta",11)] $
        buildPairsStringLength ["ana", "este", "o", "programatoare", "foarte", "interesanta"]
    , testVal "buildPairsStringLength2" [("ana",3),("este",4),("o",1), ("programatoare",13),("foarte",6),("interesanta",11)] $
        buildPairsStringLength2 ["ana", "este", "o", "programatoare", "foarte", "interesanta"]
    ]

{-
    3. Implementați, folosind obligatoriu list-comprehensions, operații pe mulțimi:
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

-- Verificare: check3
check3 :: TestData
check3 = tests_ 3
    [ testSet "setIntersection" [1, 2, 6] $ setIntersection a b
    , testSet "setDiff" [3, 7] $ setDiff a b
    , testVal "cartProduct" [(1, 3), (1, 4), (1, 5), (2, 3), (2, 4), (2, 5)] $ cartProduct [1, 2] [3, 4, 5]
    , testSet "setUnion" [1, 2, 3, 4, 6, 7, 8, 10] $ setUnion a b
    ]
  where 
      a = [1, 7, 3, 6, 2]
      b = [2, 8, 6, 10, 4, 1]


{-
    4. Experimentați în consolă funcționalitățile funcției "trace", aplicată în funcțiile "evens", "squaredEvens" și "sumOfSquaredEvens".

    Funcția "trace" primește doi parametri: un mesaj și o expresie. Rezultatul este dat de al doilea parametru (evaluarea expresiei),
    iar mesajul e afișat în momentul în care se forțează evaluarea aplicației lui trace.

    Notă: e folosit foldl' în loc de foldl pentru că primul forțează evaluarea la fiecare actualizare a acumulatorului.
    Cu foldl, toată suma ar fi calculată de-abia la sfârșit.
-}
evens :: [Int]
evens = filter (\x -> trace ("even? " ++ show x) (even x)) [0..9]

squaredEvens :: [Int]
squaredEvens = map (\x -> trace ("square " ++ show x) x^2) evens

sumOfSquaredEvens :: Int
sumOfSquaredEvens = foldl' (\acc x -> trace ("add " ++ show x) (acc + x)) 0 squaredEvens

-- Verificare: check4
check4 :: TestData
check4 = tests_ 4
    [ testManually "trace in consola" True ]

{-
    5. Scrieți o funcție "infiniteApply" care primește ca parametru o funcție "f"
    și o valoare inițială x0.
    
    Funcția ar trebui să întoarcă o listă infinită sub forma:
      x0, f x0, f (f x0), f (f (f x0)), ...
    
    Când testați funcția, fiți pregătiți să apăsați CTRL + C deoarece evaluarea
    aplicației va genera mult output sau să folosiți take.

    Notă: această funcție este echivalentă cu funcția iterate.
 -}

infiniteApply :: (Double -> Double) -> Double -> [Double]
infiniteApply f x0 = x0 : (infiniteApply f $ f x0)

-- Verificare: check5
check5 :: TestData
check5 = tests_ 5
    [ testVal "infiniteApply increment" [0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0] $ (take 10 $ infiniteApply (\x -> x + 1) 0)
    , testVal "infiniteApply powers of 2" [1.0,2.0,4.0,8.0,16.0,32.0,64.0,128.0,256.0,512.0, 1024.0,2048.0,4096.0,8192.0,16384.0,32768.0] $ (take 16 $ infiniteApply (\x -> x * 2) 1)
    ]

{-
    6. Avem o funcție "f" și derivata acesteia "df".
    
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

-- Verificare: check6
check6 :: TestData
check6 = tests_ 6
    [ testVal "f 6" 0 $ f 6
    , testVal "f 0" 36 $ f 0
    , testVal "df 6" (-12) $ df 6
    , testVal "df 0" 0 $ df 0
    ]

{-
    7. Pentru determinarea unei rădăcini a unei funcții, vom calcula aproximări
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
 -}

newtonRaphson x g dg = iterate (\x -> x - (g x) / (dg x)) x

-- Verificare: check7
check7 :: TestData
check7 = tests_ 7
    [ testVal "newtonRaphson 6 f df" [6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0] $ (take 10 $ newtonRaphson 6 f df) ]

{-
    8. Determinați soluția ecuației f(x) = 0.

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
newtonRaphsonSolve x_old atol g dg = result
  where
    nr = newtonRaphson x_old g dg
    vals = zip (tail nr) nr
    (_, result) : _ = dropWhile (\(x, y) -> abs(x - y) > atol) vals

-- Verificare: check8
check8 :: TestData
check8 = tests_ 8
    [ testVal "newtonRaphsonSolve -3 0 f df" (-6) $ newtonRaphsonSolve (-3) 0 f df
    , testVal "newtonRaphsonSolve 3 0 f df" 6 $ newtonRaphsonSolve 3 0 f df
    ]


{-
    9. Metoda Babiloniană - folosită pentru aflarea rădăcinii pătrate a unui
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

-- Verificare: check9
check9 :: TestData
check9 = tests_ 9
    [ testVal "babylonianMethod 36 3 0" 6 $ babylonianMethod 36 3 0
    , testVal "babylonianMethod 36 (-3) 0" (-6) $ babylonianMethod 36 (-3) 0
    ]
 
{-
Helpers for testing :) You can also run check1, check2 etc independently
-}
check = quickCheck False [check1, check2, check3, check4, check5, check6, check7, check8, check9]