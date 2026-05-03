module List where

-- Ascundem cele două funcții predefinite, pentru a le putea redefini noi înșine
import Prelude hiding (sum, length)

-- Exemple din textul laboratorului

data ListFolder a b c = ListFolder
    { foldNull :: c
    , foldCons :: a -> b -> c
    }

foldList :: ListFolder a b b -> [a] -> b
foldList folder = foldr (foldCons folder) (foldNull folder)

sum :: Num a => [a] -> a
sum = foldList sumFolder

sumFolder :: Num a => ListFolder a a a
sumFolder = ListFolder
    { foldNull = 0
    , foldCons = (+)
    }

length :: [a] -> Int
length = foldList lengthFolder

lengthFolder :: ListFolder a Int Int
lengthFolder = ListFolder
    { foldNull = 0
    , foldCons = const (+ 1)
    }

infixl 5 <+>
(<+>) :: ListFolder a b b
      -> ListFolder a c c
      -> ListFolder a (b, c) (b, c)
f <+> g = ListFolder
    { foldNull = (foldNull f, foldNull g)
    , foldCons = \a (b, c) -> (foldCons f a b, foldCons g a c)
    }

sumLength :: Num a => [a] -> (a, Int)
sumLength = foldList (sumFolder <+> lengthFolder)

-- steep :: (Ord a, Num a) => [a] -> Bool
-- steep [] = True
-- steep (x : xs) = x > sum xs && steep xs

infixl 5 >.>
(>.>) :: ListFolder a b b
      -> ListFolder a (b, c) c
      -> ListFolder a (b, c) (b, c)
f >.> g = ListFolder
    { foldNull = (foldNull f, foldNull g)
    , foldCons = \a (b, c) -> (foldCons f a b, foldCons g a (b, c))
    }

steep ::(Ord a, Num a) => [a] -> (a, Bool)
steep = foldList (sumFolder >.> steepFolder)

steepFolder :: Ord a => ListFolder a (a, Bool) Bool
steepFolder = ListFolder
    { foldNull = True
    , foldCons = \x (s, stp) -> x > s && stp
    }

-- evenSum :: Num a => [a] -> a
-- evenSum [] = 0
-- evenSum (x : xs) = x + oddSum xs

-- oddSum :: Num a => [a] -> a
-- oddSum [] = 0
-- oddSum (x : xs) = evenSum xs

evenSumFolder :: Num a => ListFolder a (a, a) a
evenSumFolder = ListFolder
    { foldNull = 0
    , foldCons = \x (evenS, oddS) -> x + oddS
    }

oddSumFolder :: Num a => ListFolder a (a, a) a
oddSumFolder = ListFolder
    { foldNull = 0
    , foldCons = \x (evenS, oddS) -> evenS
    }

infixl 5 <.>
(<.>) :: ListFolder a (b, c) b
      -> ListFolder a (b, c) c
      -> ListFolder a (b, c) (b, c)
f <.> g = ListFolder
    { foldNull = (foldNull f, foldNull g)
    , foldCons = \a (b, c) -> (foldCons f a (b, c), foldCons g a (b, c))
    }

evenOddSums :: Num a => [a] -> (a, a)
evenOddSums = foldList (evenSumFolder <.> oddSumFolder)

{-
Variație a lui steep, care utilizează media restului listei în loc de sumă.

Ilustrează combinarea a trei foldere.
-}
steepMean :: (Ord a, Fractional a) => [a] -> ((a, Int), Bool)
steepMean = foldList ((sumFolder <+> lengthFolder) >.> steepMeanFolder)

steepMeanFolder :: (Ord a, Fractional a) => ListFolder a ((a, Int), Bool) Bool
steepMeanFolder = ListFolder
    { foldNull = True
    , foldCons = \x ((s, l), stp) -> x > s / fromIntegral l && stp
    }
