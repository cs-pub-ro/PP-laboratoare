{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Maybe
import Data.List (sort, insertBy)
import Data.Function (on)
import TestPP


{-
    De rezolvat in urmatoarea ordine:
    1) Show
    2) Eq
    3) Ord
    4) Invertible
    5) Functor
    6) Foldable
    7) Container
    8) size cu foldr
-}

class Container t where
    contents :: t a -> [a]

class Invertible a where
    invert :: a -> a    

data BST a = BSTNod a (BST a) (BST a) | BSTNil

insertElem :: (Ord a, Eq a) => BST a -> a -> BST a
insertElem BSTNil elem = BSTNod elem BSTNil BSTNil
insertElem root@(BSTNod value left right) elem
  | value == elem = root
  | value < elem = BSTNod value left (insertElem right elem)
  | value > elem = BSTNod value (insertElem left elem) right  

findElem :: (Ord a, Eq a) => BST a -> a -> Maybe a
findElem BSTNil _ = Nothing
findElem (BSTNod value left right) elem
  | value == elem = Just value
  | value < elem = findElem right elem
  | value > elem = findElem left elem

size :: (BST a) -> Int
size BSTNil = 0
size (BSTNod _ left right) = 1 + (size left) + (size right)

height :: (BST a) -> Int
height BSTNil = 0
height (BSTNod elem left right) = 1 + max (height left) (height right)

inorder :: BST a -> [a]
inorder BSTNil = []
inorder (BSTNod elem left right) = (inorder left) ++ [elem] ++ (inorder right) 

root = foldl insertElem BSTNil [7, 4, 12, 2, 3, 1, 10, 15, 8]

instance Eq a => Eq (BST a) where
    (==) tree1 tree2 = undefined

check1 :: TestData
check1 = tests_ 1 $
    [
        testVal "Eq 1" True $ root == root,
        testVal "Eq 2" False $ root == insertElem root 20,
        testVal "Eq 3" False $ root == BSTNil
    ]

instance Show a => Show (BST a) where
    show tree = undefined

check2 :: TestData
check2 = tests_ 2 $
    [
        testVal "show tree" "7\n\t4\n\t\t2\n\t\t\t1\n\t\t\t3\n\t12\n\t\t10\n\t\t\t8\n\t\t15\n" $ show root
    ]

instance Ord a => Ord (BST a) where
    (<=) t1 t2 = undefined

check3 :: TestData
check3 = tests_ 3 $
    [ 
        testVal "Ord 1" True $ root >= root,
        testVal "Ord 2" True $ root <= root,
        testVal "Ord 3" False $ root > root,
        testVal "Ord 4" True $ foldl insertElem root [10, 20, 22] > root,
        testVal "Ord 5" True $ BSTNil < root
    ]

instance Invertible (BST a) where
    invert tree = undefined

check4 :: TestData
check4 = tests_ 4 $
    [ 
        testVal "Invertible 1" True $ root == invert (invert root)
    ]

instance Functor BST where
    fmap f tree = undefined

check5 :: TestData
check5 = tests_ 5 $
    [
        testVal "Functor 1" BSTNil $ fmap (+1) BSTNil,
        testVal "Functor 2" (insertElem BSTNil 6) $ fmap (+1) (insertElem BSTNil 5),
        testVal "Functor 3" (foldl insertElem BSTNil (map (+10) [7, 4, 12, 2, 3, 1, 10, 15, 8])) $ fmap (+10) root
    ]

instance Foldable BST where
    foldr f acc tree = undefined

check6 :: TestData
check6 = tests_ 6 $
    [
        testVal "Foldable 1" 62 $ foldr (+) 0 root,
        testVal "Foldable 2" 82 $ foldr (+) 0 (insertElem root 20)
    ]

instance Container BST where
    contents tree = undefined

check7 = tests_ 7 $
    [
        testVal "Container 1" 0 $ length (contents BSTNil),
        testVal "Container 2" [15,12,10,8,7,4,3,2,1] $ contents root
    ]

sizeFold :: (BST a) -> Int
sizeFold tree = undefined

check8 = tests_ 8 $
    [
        testVal "sizeFold 1" 0 $ sizeFold BSTNil,
        testVal "sizeFold 2" 9 $ sizeFold root
    ]

check = quickCheck False [check1, check2, check3, check4, check5, check6, check7, check8]