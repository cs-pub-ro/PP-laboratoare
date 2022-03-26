{-
List comprehensions. Observați următoarele exemple și asemănarea cu limbajul
matematic.

Folosiți `take` pentru a limita numărul de valori afișate :)
-}

-- {x | x <- N }
naturals = [x | x <- [0 ..]]

-- {(x, y) | x <- N, y <- N, x < y }
increasingPairs = [(x, y) | x <- naturals, y <- naturals, x < y]
-- Funcția ^ nu va genera perechi `(1, _)`, ... deoarece valorile din `y` sunt
-- de o cardinalitate infinită.

increasingPairs2 = [(x, y) | x <- naturals, y <- [x+1 ..]]
-- mai sus se generează perechile direct crescător

-- {x! | x <- N}
productList [] = 1
productList (x:xs) = x * productList xs

factorials = [factorial x | x <- naturals]
factorial n = productList [1 .. n]

{-
Cum se traduc list-comprehensions în expresii simple cu funcționale?

O expresie de forma

    [expr | x <- listX, testX, y <- listY, testXY, ...]

este evaluată prin selectarea consecutivă a elementelor din `listX`, testarea
predicatului `testX`, selectarea din `listY` (dacă `testX` este satisfăcut),
... Varianta cu funcționale ar arăta

    map (\x y ... -> expr) $ ... $ filter testXY $ zip listY $ filter testX listX

Funcțiile de mai sus, se traduc -- după ceva simplificări -- în:
-}
naturals' = [0 ..] -- map id $ filter (\x -> True) [0 ..]
increasingPairs' = filter (\(y, x) -> x < y) $ zip naturals naturals
-- ^ observați folosirea `zip` și o funcție uncurry pentru `filter`
factorials' = map factorial naturals
