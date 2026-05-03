module BST where

import TestPP

data BST a
    = BSTNod { vl :: a, lt :: BST a, rt :: BST a}
    | BSTNil

insertElem :: (Ord a, Eq a) => BST a -> a -> BST a
insertElem BSTNil elem = BSTNod elem BSTNil BSTNil
insertElem root@(BSTNod value left right) elem
  | value == elem = root
  | value < elem = BSTNod value left (insertElem right elem)
  | value > elem = BSTNod value (insertElem left elem) right

{-
Calculează înălțimea arborelui (vezi laboratorul anterior).
-}
height :: BST a -> Int
height BSTNil = 0
height (BSTNod elem left right) = 1 + max (height left) (height right)

{-
Verifică dacă un arbore este echilibrat.

Transformarea nu este compoizițională, pentru că left și right contribuie
nu numai prin imaginea lor sub isBalanced, ci și prin imaginea lor sub height.
Acest lucru conduce la o eficiență scăzută, întrucât subraborii sunt parcurși
repetat de isBalanced și de height.
-}
isBalanced :: BST a -> Bool
isBalanced BSTNil = True
isBalanced (BSTNod _ left right) =
    isBalanced left
    && isBalanced right
    && abs (height left - height right) <= 1

{-
Ex. 1

Utilizați tupling și equational reasoning pentru a transforma isBalanced într-o 
variantă compozițională, numită heightIsBalanced, care să parcurgă arborele o 
singură dată. Mai precis, impuneți proprietatea

heightIsBalanced bst = (heigth bst, isBalanced bst).

Pe cazuri, derivați o definiție mai eficientă a funcției heightIsBalanced.

DERIVARE: ...

heightIsBalanced BSTNil
= (height BSTNil, isBalanced BSTNil)
= (0, True)

heightIsBalanced (BSTNod elem left right)
= (height (BSTNod elem left right), isBalanced (BSTNod elem left right))
= ( 1 + max (height left) (height right)
  , isBalanced left
    && isBalanced right
    && abs (height left - height right) <= 1
  )
= let (heightLeft, isBalancedLeft) = heightIsBalanced left
      (heightRight, isBalancedRight) = heightIsBalanced right
  in ( 1 + max heightLeft heightRight
     , isBalancedLeft && isBalancedRight && abs (heightLeft - heightRight) <= 1
     )
-}
heightIsBalanced :: BST a -> (Int, Bool)
heightIsBalanced BSTNil = (0, True)
heightIsBalanced (BSTNod elem left right) = 
    ( 1 + max heightLeft heightRight
    , isBalancedLeft && isBalancedRight && abs (heightLeft - heightRight) <= 1
    )
  where
    (heightLeft,  isBalancedLeft)  = heightIsBalanced left
    (heightRight, isBalancedRight) = heightIsBalanced right

check1 :: TestData
check1 = tests_ 1 
    [
        testVal "heightIsBalanced 1" (3, True) $
            heightIsBalanced balanced,
        testVal "heightIsBalanced 2" (10, False) $
            heightIsBalanced notBalanced
    ]
  where
    notBalanced = foldl insertElem BSTNil [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    balanced = foldl insertElem BSTNil [5, 3, 7, 1, 4, 6, 8]

{-
Din păcate, deși heightIsBalanced este compozițională, ea nu poate fi 
implementată cu foldr, întrucât cea din urmă expune o vedere liniară asupra 
arborelui, ascunzând structura sa ierarhică.

În plus, deși heightIsBalanced este mai eficientă decât isBalanced, pare că 
prețul plătit pentru eficiența sporită este pierderea modularității și a 
reutilizării, întrucât funcția height a trebuit reimplementată în definiția lui 
heightIsBalanced.

În vederea beneficierii atât de eficiență, cât și de modularitate/reutilizare,
soluția este definirea unui mecanism mai expresiv de reducere (folding) a 
arborelui, care să expună structura sa ierarhică.

Tipul de date BSTFolder reprezintă o colecție de funcții care permit reducerea 
diverselor forme pe care le poate lua arborele, presupunând că subarborii au 
fost deja reduși recursiv.

Semnificația variabilelor de tip (vezi textul laboratorului):

* a = tipul nodurilor din arbore (la fel ca în BST a)
* b = tipul rezultatului reducerii subarborilor
* c = tipul rezultatului reducerii întregului arbore
-}
data BSTFolder a b c = BSTFolder
    { foldNil :: c
    , foldNod :: a -> b -> b -> c
    }

foldBST :: BSTFolder a b b -> BST a -> b
foldBST folder = go
  where
    go BSTNil = foldNil folder
    go (BSTNod elem left right) = foldNod folder elem (go left) (go right)

{-
Reimplementarea lui height folosind acest mecanism mai expresiv (vezi textul
laboratorului).
-}
heightWithFold :: BST a -> Int
heightWithFold = foldBST heightFolder

heightFolder :: BSTFolder a Int Int
heightFolder = BSTFolder
    { foldNil = 0
    , foldNod = \_ leftHeight rightHeight -> 1 + max leftHeight rightHeight
    }

{-
Operatorul (>.>) combină două foldere SEMIDEPENDENTE, care reduc arborele la 
tipuri diferite (b și c), într-un folder care reduce arborele la o pereche de 
tipuri (b, c) (vezi textul laboratorului).

Parantezele unghiulare indică sensul dependenței, de la stânga la dreapta.
Primul folder este independent, reducând graful la tipul b, în timp ce al 
doilea folder este dependent de primul, și reduce graful la tipul c, pornind
de la informația calculată de AMBELE foldere, (b, c).

infixl (l = left) asigură asociativitatea la stânga a operatorului.
-}
infixl 5 >.>
(>.>) :: BSTFolder a b b
      -> BSTFolder a (b, c) c
      -> BSTFolder a (b, c) (b, c)
f >.> g = BSTFolder
    { foldNil = (foldNil f, foldNil g)
    , foldNod = \a (b1, c1) (b2, c2) ->
        (foldNod f a b1 b2, foldNod g a (b1, c1) (b2, c2))
    }

{-
Ex. 2

Reimplementați funcția isBalanced folosind noul mecanism de reducere. 
Propriu-zis, trebuie să implementați doar BSTFolder-ul.

Boolean-ul produs de folder depinde nu numai de mulțimile de boleenii calculați 
pentru subarbori, ci și de înălțimile calculate pentru aceiași subarbori.
Din acest motiv, funcțiile din folder iau ca parametri perechi între o înălțime
și un boolean, (Int, Bool).

Observați că acum nu a mai trebuit să reimplementăm funcționalitatea lui 
height, ca în funcția heightIsBalanced de mai sus. Am putut reutiliza 
funcționalitatea lui height, dar nu la nivelul întregii funcții, ci doar la 
nivelul folderului său.
-}
isBalancedWithFold :: BST a -> Bool
isBalancedWithFold = snd . foldBST (heightFolder >.> isBalancedFolder)

isBalancedFolder :: BSTFolder a (Int, Bool) Bool
isBalancedFolder = BSTFolder
    { foldNil = True
    , foldNod = \elem
                 (heightLeft,  isBalancedLeft)
                 (heightRight, isBalancedRight) ->
                    isBalancedLeft
                    && isBalancedRight
                    && abs (heightLeft - heightRight) <= 1
    }

check2 :: TestData
check2 = tests_ 2 
    [
        testVal "isBalancedWithFold 1" True $
            isBalancedWithFold balanced,
        testVal "isBalancedWithFold 2" False $
            isBalancedWithFold notBalanced
    ]
  where
    notBalanced = foldl insertElem BSTNil [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    balanced = foldl insertElem BSTNil [5, 3, 7, 1, 4, 6, 8]

{-
Funcția de parcurgere în inordine din laboratorul anterior.

Deși poate nu este evident, complexitatea acestei funcții este supraliniară.
Notăm cu T(n) numărul de pași realizați pentru un abore de dimensiune n.

În cazul cel mai favorabil, în care arborele degenerează la o listă spre dreapta
(toți subarborii stângi sunt vizi), obținem:

T(n) = T(n - 1) + Theta(1),

cu soluția T(n) = Theta(n).

În schimb, în cazul în care arborele este echilibrat, obținem:

T(n) = 2T(n/2) + Theta(n/2),

întrucât complexitatea concatenării este proporțională cu dimensiunea primei 
liste. Soluția este T(n) = Theta(n log n).

În cazul cel mai defavorabil, în care arborele degenerează la o listă spre 
stânga (toți subarborii drepți sunt vizi), obținem:

T(n) = T(n - 1) + Theta(n - 1),

cu soluția T(n) = Theta(n^2).
-}
inorder :: BST a -> [a]
inorder BSTNil = []
inorder (BSTNod elem left right) = inorder left ++ [elem] ++ inorder right

{-
Ex. 3

Utilizați tehnica acumulării și equational reasoning pentru a obține o 
definiție liniară a lui inorder, numită inorderAcc. Mai precis, impuneți 
proprietatea:

inorderAcc node acc = inorder node ++ acc,

de unde rezultă că

inorder node = inorderAcc node [].

Pe cazuri, derivați o definiție mai eficientă a lui inorderAcc.

DERIVARE:

inorderAcc BSTNil acc
= inorder BSTNil ++ acc
= [] ++ acc
= acc

inorderAcc (BSTNod elem left right) acc
= (inorder left ++ [elem] ++ inorder right) ++ acc
= (inorder left ++ elem : inorder right) ++ acc
= inorder left ++ (elem : inorder right ++ acc)
= inorder left ++ elem : (inorder right ++ acc)
= inorder left ++ elem : inorderAcc right acc
= inorderAcc left (elem : inorderAcc right acc)
-}
inorderLinear :: BST a -> [a]
inorderLinear node = inorderAcc node []
  where
    inorderAcc BSTNil acc = acc
    inorderAcc (BSTNod elem left right) acc =
        inorderAcc left (elem : inorderAcc right acc)

check3 :: TestData
check3 = tests_ 3
    [
        testSet "inorderLinear" values $ inorderLinear root
    ]
  where
    root = foldl insertElem BSTNil [7, 4, 12, 2, 3, 1, 10, 15, 8]
    values = [1, 2, 3, 4, 7, 8, 10, 12, 15]

check = quickCheck False [check1, check2, check3]
