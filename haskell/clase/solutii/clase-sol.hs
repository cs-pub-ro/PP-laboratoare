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

sizeFold :: (BST a) -> Int
sizeFold tree = foldr (\_ acc -> acc + 1) 0 tree

height :: (BST a) -> Int
height BSTNil = 0
height (BSTNod elem left right) = 1 + max (height left) (height right)

inorder :: BST a -> [a]
inorder BSTNil = []
inorder (BSTNod elem left right) = (inorder left) ++ [elem] ++ (inorder right) 

printLevel :: Show a => Char -> Int -> BST a -> [Char]
printLevel _ _ BSTNil = ""
printLevel tab level (BSTNod root left right) = (replicate level tab) ++ (show root) ++ "\n" 
    ++ (printLevel tab (level + 1) left) 
    ++ (printLevel tab (level + 1) right) 
  
instance Show a => Show (BST a) where
    show BSTNil = ""
    show (BSTNod root left right) = printLevel '\t' 0 (BSTNod root left right)

instance Eq a => Eq (BST a) where
    (==) BSTNil BSTNil = True
    (==) (BSTNod e1 l1 r1) (BSTNod e2 l2 r2) = e1 == e2 && l1 == l2 && r1 == r2
    (==) _ _ = False

instance Ord a => Ord (BST a) where
    (<=) t1 t2 = height t1 <= height t2
    (<) t1 t2 = height t1 < height t2

instance Invertible (BST a) where
    invert BSTNil = BSTNil
    invert (BSTNod a left right) = BSTNod a (invert right) (invert left)

instance Functor BST where
    fmap f BSTNil = BSTNil
    fmap f (BSTNod a left right) = BSTNod (f a) (fmap f left) (fmap f right)

instance Foldable BST where
    foldr f acc BSTNil = acc
    foldr f acc (BSTNod value left right) = foldr f (f value newAcc) left
        where newAcc = foldr f acc right

instance Container BST where
    contents tree = reverse $ foldr (:) [] tree

check = quickCheck False []