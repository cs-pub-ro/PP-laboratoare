module List () where

-- Ascundem cele două funcții predefinite, pentru a le putea redefini noi înșine
import Prelude hiding (sum, length)

-- Exemple din textul laboratorului

data Folder a b c = Folder
    { foldNull :: c
    , foldCons :: a -> b -> c
    }

fold :: Foldable t => Folder a b b -> t a -> b
fold folder = foldr (foldCons folder) (foldNull folder)

sum :: (Foldable t, Num a) => t a -> a
sum = fold sumFolder

sumFolder :: Num a => Folder a a a
sumFolder = Folder
    { foldNull = 0
    , foldCons = (+)
    }

length :: Foldable t => t a -> Int
length = fold lengthFolder

lengthFolder :: Folder a Int Int
lengthFolder = Folder
    { foldNull = 0
    , foldCons = const (+ 1)
    }

infixl 5 <+>
(<+>) :: Folder a b b
      -> Folder a c c
      -> Folder a (b, c) (b, c)
f <+> g = Folder
    { foldNull = (foldNull f, foldNull g)
    , foldCons = \a (b, c) -> (foldCons f a b, foldCons g a c)
    }

sumLength :: (Foldable t, Num a) => t a -> (a, Int)
sumLength = fold (sumFolder <+> lengthFolder)

-- steep :: (Ord a, Num a) => [a] -> Bool
-- steep [] = True
-- steep (x : xs) = x > sum xs && steep xs

infixl 5 >.>
(>.>) :: Folder a b b
      -> Folder a (b, c) c
      -> Folder a (b, c) (b, c)
f >.> g = Folder
    { foldNull = (foldNull f, foldNull g)
    , foldCons = \a (b, c) -> (foldCons f a b, foldCons g a (b, c))
    }

steep :: (Ord a, Num a) => [a] -> (a, Bool)
steep = fold (sumFolder >.> steepFolder)

steepFolder :: Ord a => Folder a (a, Bool) Bool
steepFolder = Folder
    { foldNull = True
    , foldCons = \x (s, stp) -> x > s && stp
    }

-- evenSum :: Num a => [a] -> a
-- evenSum [] = 0
-- evenSum (x : xs) = x + oddSum xs

-- oddSum :: Num a => [a] -> a
-- oddSum [] = 0
-- oddSum (x : xs) = evenSum xs

evenSumFolder :: Num a => Folder a (a, a) a
evenSumFolder = Folder
    { foldNull = 0
    , foldCons = \x (evenS, oddS) -> x + oddS
    }

oddSumFolder :: Num a => Folder a (a, a) a
oddSumFolder = Folder
    { foldNull = 0
    , foldCons = \x (evenS, oddS) -> evenS
    }

infixl 5 <.>
(<.>) :: Folder a (b, c) b
      -> Folder a (b, c) c
      -> Folder a (b, c) (b, c)
f <.> g = Folder
    { foldNull = (foldNull f, foldNull g)
    , foldCons = \a (b, c) -> (foldCons f a (b, c), foldCons g a (b, c))
    }

evenOddSums :: Num a => [a] -> (a, a)
evenOddSums = fold (evenSumFolder <.> oddSumFolder)

{-
Variație a lui steep, care utilizează media restului listei în loc de sumă.

Ilustrează combinarea a trei foldere.
-}
steepMean :: (Ord a, Fractional a) => [a] -> ((a, Int), Bool)
steepMean = fold ((sumFolder <+> lengthFolder) >.> steepMeanFolder)

steepMeanFolder :: (Ord a, Fractional a) => Folder a ((a, Int), Bool) Bool
steepMeanFolder = Folder
    { foldNull = True
    , foldCons = \x ((s, l), stp) -> x > s / fromIntegral l && stp
    }
