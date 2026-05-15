# Haskell: Raționamente funcționale

- Data publicării: 12.05.2026
- Data ultimei modificări: 15.05.2026

## Obiective

Scopul acestui laborator este aprofundarea unui stil de raționament specific programării funcționale, apropiat de cel matematic, denumit ***equational reasoning*** (ER).

Aspectele urmărite sunt:

- definirea ER
- prezentarea unor proprietăți ale funcționalelor uzuale
- aplicarea ER pentru demonstrarea unor proprietăți ale funcțiilor
- aplicarea ER pentru optimizarea implementărilor funcțiilor

## Introducere

Stilul declarativ al programelor funcționale, inspirat din matematică, permite tratarea definițiilor și proprietăților funcțiilor ca **ecuații**, și manipularea expresiilor de program pornind de la aceste ecuații. Această abordare poartă numele de ***equational reasoning*** (ER). Ea permite **demonstrarea** de proprietăți noi ale funcțiilor și chiar **optimizarea** implementării acestora, utilizând **același limbaj** cu cel în care sunt definite expresiile însele. În limbajele imperative, este de obicei necesară introducerea unei **alte notații** logice pentru exprimarea proprietăților și demonstrarea lor.

Contact cu ER am avut deja în contextul **modelului de evaluare** din calculul lambda, bazat pe substituție textuală. Plecând de la definiția `f x = x + 4`, valoarea aplicației `f (1 + 2)` se obține prin ER, ținând cont și de evaluarea leneșă din Haskell, astfel: `f (1 + 2) = (1 + 2) + 4 = 3 + 4 = 7`. De asemenea, ER este folosit implicit în demonstrațiile bazate pe **inducție structurală**. Vom explora în cadrul laboratorului și alte utilizări ale conceptului.

## Proprietăți ale funcționalelor

Pentru a avea o bază de plecare mai consistentă în exemplificarea ER, surprindem câteva proprietăți ale funcționalelor uzuale, care pot fi demonstrate prin inducție structurală.

Proprietățile funcționalei `map` pot fi exprimate ca **ecuații funcționale** (în care termenii sunt funcții):

```haskell
map id = id
map (f . g) = map f . map g
```

Prima proprietate afirmă că aplicarea funcției identitate pe fiecare element este echivalentă cu aplicarea funcției identitate pe întreaga listă. A doua proprietate afirmă că două aplicări succesive ale unor funcții pe fiecare element al unei liste sunt echivalente cu o singură aplicare a funcției compuse pe fiecare element al listei.

În cazul funcționalei `foldr`, am intrat deja în contact cu proprietatea de ***universalitate*** încă din primul laborator despre funcționale în Racket (deși nu am numit-o astfel), care afirmă că **unica** soluție a ecuațiilor de mai jos care definesc funcția `g`,

```haskell
g [] = acc
g (x : xs) = f x (g xs)
```

este

```haskell
g = foldr f acc
```

Cu alte cuvinte, `foldr` surpinde exact așa-zisele transformări **compoziționale** pe liste, în sensul că rezultatul lui `g (x : xs)` depinde de `xs` exclusiv prin imaginea sa sub `g`, adică `g xs`. `g` nu se poate aplica asupra altui parametru decât `xs`, iar `xs` nu poate să apară ca parametru pentru altă funcție decât `g`.

În plus, introducem proprietatea de ***fuziune***, care ne spune în ce condiții transformarea rezultatului lui `foldr` printr-o altă funcție poate fi scrisă direct cu `foldr`:

```haskell
h . foldr f a = foldr g b
```

Condițiile sunt următoarele (a doua e doar suficientă, nu și necesară):

```haskell
h a = b
h (f x y) = g x (h y)
```

## Demonstrarea de proprietăți

O primă aplicație a ER o constituie demonstrarea de noi proprietăți ale funcțiilor.

Pentru a exemplifica valorificarea proprietăților funcționalei **`map`**, să presupunem următoarele definiții de funcții:

```haskell
incDouble   = map ((+ 1) . (* 2))
doubleHeads = map ((* 2) . head)

incDoubleHeads1 = incDouble . map head
incDoubleHeads2 = map (+ 1) . doubleHeads
```

Vrem să demonstrăm că `incDoubleHeads1` și `incDoubleHeads2` sunt **aceeași funcție**. Putem aplica ER în felul următor:

```haskell
incDoubleHeads1
= {- definiție incDoubleHeads1 -}
incDouble . map head
= {- definiție incDouble -}
map ((+ 1) . (* 2)) . map head
= {- proprietate 2 map -}
map ((+ 1) . (* 2) . head)

incDoubleHeads2
= {- definiție incDoubleHeads2 -}
map (+ 1) . doubleHeads
= {- definiție doubleHeads -}
map (+ 1) . map ((* 2) . head)
= {- proprietate 2 map -}
map ((+ 1) . (* 2) . head)
```

Din moment ce obținem aceeași expresie finală (utilizând și asociativitatea compunerii), egalitatea funcțiilor este demonstrată. Observați că întregul raționament se desfășoară în acest caz la nivelul funcțiilor.

În continuare, exemplificăm valorificarea proprietății de **fuziune** a funcționalei **`foldr`**. Ne propunem să demonstrăm **egalitatea funcțională** de mai jos, care afirmă că înmulțirea cu 2 a fiecărui element al unei liste, urmată de însumarea rezultatelor, este echivalentă cu însumarea elementelor originale, urmată de dublarea rezultatului:

```haskell
(* 2) . sum = sum . map (* 2)
```

Planul este de a aplica proprietatea de fuziune asupra ambelor părți ale ecuației, în speranța că vom obține în final rescrieri identice cu `foldr`.

Tratăm mai întâi partea stângă. Pentru a putea aplica proprietatea de fuziune, este necesară **rescrierea** lui `sum` utilizând `foldr`.

```haskell
(* 2) . sum
= {- rescriere sum -}
(* 2) . foldr (+) 0
```

În acest moment, putem instanția variabilele `h`, `f` și `a` din descrierea proprietății:

```haskell
h = (* 2)
f = (+)
a = 0
```

Pentru a rescrie întreaga compunere ca `foldr g b`, trebuie să particularizăm condițiile din descrierea proprietății:

```haskell
b = h a = (* 2) 0 = 0

h (f x y) = g x (h y)
(* 2) ((+) x y) = g x ((* 2) y)
2 * (x + y) = g x (2 * y)
2 * x + 2 * y = g x (2 * y)
```

Dacă în ultima linie generalizăm `2 * y = acc`, se obține definiția lui `g`:

```haskell
g x acc = 2 * x + acc
```

De aici, rezultă rescrierea întregii părți stângi a ecuații inițiale:

```haskell
(* 2) . sum
= {- rescriere sum -}
(* 2) . foldr (+) 0
= {- proprietatea de fuziune -}
foldr (\x acc -> 2 * x + acc) 0
= {- dacă vream neapărat complet point-free -}
foldr ((+) . (* 2)) 0
```

Similar, abordăm și partea dreaptă. Pentru a putea aplica proprietatea de fuziune, este necesară **rescrierea** lui `map` utilizând `foldr`.

```haskell
sum . map (* 2)
= {- rescriere map -}
sum . foldr (\x acc -> 2 * x : acc) []
```

În acest moment, putem instanția din nou variabilele `h`, `f` și `a` din descrierea proprietății:

```haskell
h = sum
f = \x acc -> 2 * x : acc
a = []
```

Pentru a rescrie întreaga compunere ca `foldr g b`, trebuie să particularizăm condițiile din descrierea proprietății:

```haskell
b = h a = sum [] = 0

h (f x y) = g x (h y)
sum (2 * x : y) = g x (sum y)
2 * x + sum y = g x (sum y)
```

Dacă în ultima linie generalizăm `sum y = acc`, se obține definiția lui `g`:

```haskell
g x acc = 2 * x + acc
```

De aici, rezultă rescrierea întregii părți drepte a ecuații inițiale:

```haskell
sum . map (* 2)
= {- rescriere map -}
sum . foldr (\x acc -> 2 * x : acc) []
= {- proprietatea de fuziune -}
foldr (\x acc -> 2 * x + acc) 0
= {- dacă vream neapărat complet point-free -}
foldr ((+) . (* 2)) 0
```

Observăm că ambele părți ale egalității originale, `(* 2) . sum = sum . map (* 2)`, sunt aduse la aceeași formă, `foldr ((+) . (* 2)) 0`; prin urmare, egalitatea este demonstrată.

De fapt, derivarea realizată pentru partea dreaptă a egalității de mai sus poate fi generalizată pentru orice aplicație a lui `map` urmată de o aplicație a lui `foldr`:

```haskell
foldr u c . map v = foldr (\x acc -> u (v x) acc) c = foldr (u . v) c
```

O derivare similară se poate realiza pentru `foldr u c . filter p`.

## Optimizarea implementărilor

În secțiunea anterioară, ne-am concentrat pe demonstrarea proprietăților funcțiilor, aspect important în asigurarea corectitudinii programelor. În mod interesant, rescrierile realizate în acest scop au produs și un **efect neașteptat**: am obținut pe alocuri definiții **mai eficiente** ale acestor funcții!

De exemplu, funcțiile `incDoubleHeads1/2` sunt inițial definite drept compuneri a două instanțe de `map`. Acest lucru poate avea consecințe diferite, în raport cu strategia de evaluare a limbajului:

- În cazul evaluării **imediate** (*eager*), ca în Racket, primul `map` construiește **în întregime** o listă intermediară, consumată apoi de al doilea `map`. Dacă lista originală are 100 de elemente, primul `map` construiește mai întâi o listă intermediară de 100 de elemente, **simultan** prezente în memorie, după care al doilea `map` construiește o a doua listă de 100 de elemente, **simultan** prezente în memorie.
- În cazul evaluării **leneșe**, surprinsă de fluxurile din Racket, și implicit în Haskell, primul `map` construiește, **pe rând**, câte o **singură** celulă de listă la un moment dat, oferită imediat celui de-al doilea `map`. Memoria ocupată de celula intermediară poate fi **eliberată** imediat ce aceasta a fost consumată de al doilea `map`. Prin urmare, evaluarea decurge **mai eficient** ca în cazul *eager* de mai sus, pentru că cele 100 de elemente **nu** mai trebuie să existe simultan în memorie, cu toate că celulele intermediare tot trebuie construite de primul `map` (chiar dacă pe rând, nu toate deodată).

Să observăm ce se întâmplă în cazul variantei obținute prin ER pornind de la funcțiile noastre: `map ((+ 1) . (* 2) . head)` parcurge lista o **singură** dată, cu o **eficiență mai mare** decât în **ambele** cazuri de mai sus:

- În cazul evaluării **imediate** (*eager*), se construiește o **singură** listă de 100 de elemente, **simultan** prezente în memorie, fără a mai fi necesară lista intermediară de 100 de elemente, simultan prezente în memorie.
- În cazul evaluării **leneșe**, celulele intermediare nu mai sunt construite nici măcar pe rând, iar unicul `map` produce câte o celulă a listei rezultate, pe rând.

Similar, în exemplul despre egalitatea `(* 2) . sum = sum . map (* 2)`, partea **stângă** este **mai eficientă**, întrucât **nu** mai este necesară crearea celulelor intermediare de către `map`, ca în partea dreaptă.

### Acumulare prin parametri

După cum știm încă din laboratorul despre tipurile de recursivitate, varianta **pe coadă** utilizează parametri suplimentari cu rol de **acumulator**, și tinde să utilizeze spațiu **constant** în situațiile în care varianta pe stivă utilizează spațiul liniar.

De exemplu, funcția `sum` poate fi implementată cu cele două tipuri de recursivitate astfel:

```haskell
sumStack [] = 0
sumStack (x : xs) = x + sumStack xs

sumTail [] acc = acc
sumTail (x : xs) acc = sumTail xs (x + acc)
```

Un aspect interesant este că varianta pe coadă poate fi **derivată** prin ER din varianta pe stivă, presupunând că nu o cunoaștem încă. Efortul nostru inițial constă în a ne pune problema utilizării unui acumulator și în a elabora proprietatea următoare, care stabilește legătura dintre cele două implementări:

```haskell
sumTail xs acc = sumStack xs + acc
{- de unde rezultă că -}
sumStack xs = sumTail xs 0
```

Evident, dacă interpretăm prima linie de mai sus direct ca definiție computațională a lui `sumTail`, în funcție de `sumStack`, **nu** obținem implementarea pe coadă dorită și nici îmbunătățirea complexității spațiale, ci doar o nouă funcție aparent inutilă. Secretul este să valorificăm acea proprietate în cadrul unui raționament **pe cazuri**, ca să derivăm altă definiție mai eficientă:

```haskell
sumTail [] acc
= {- proprietate sumTail -}
sumStack [] + acc
= {- caz de bază sumStack -}
0 + acc
= {- 0 ca element neutru pentru adunare -}
acc

sumTail (x : xs) acc
= {- proprietate sumTail -}
sumStack (x : xs) + acc
= {- caz general sumStack -}
(x + sumStack xs) + acc
= {- comutativitatea adunării -}
(sumStack xs + x) + acc
= {- asociativitatea adunării -}
sumStack xs + (x + acc)
= {- proprietate sumTail citită invers -}
sumTail xs (x + acc)
```

După cum se observă, se obține exact definiția cunoscută a variantei pe coadă. A fost necesar să **expandăm** aplicația lui `sumTail` în termenii lui `sumStack`, dar și să o **recuperăm** la final. Cu alte cuvinte, a trebuit să citim proprietatea lui `sumTail` în raport cu `sumStack` în **ambele sensuri**.

### Îmbogățirea rezultatului

Abordarea duală la îmbogățirea listei de parametri cu acumulatori, de mai sus, este îmbogățirea rezultatului cu informație suplimentară (*tupling*), în vederea creșterii eficienței.

Să luăm ca exemplu calculul mediei elementelor unei liste:

```haskell
mean xs = sum xs / length xs
```

Deși compactă, modulară, și bazată pe reutilizare, implementarea de mai sus are **dezavantajul** că lista `xs` este parcursă **de două ori**, indiferent de strategia de evaluare. Dacă lista este foarte lungă, pot apărea penalizări importante de performanță (de exemplu, din cauza efectelor de *cache*).

Să derivăm prin ER o definiție mai eficientă, care parcurge lista **o singură dată**. Definim o funcție ajutătoare care calculează simultan suma și lungimea; proprietatea dorită este:

```haskell
sumLength xs = (sum xs, length xs)
```

La fel ca în cazul proprietății inițiale a lui `sumTail` din secțiunea anterioară, dacă interpretăm proprietatea lui `sumLength` direct ca definiție computațională, lista e în continuare parcursă de două ori, și nu obținem nicio îmbunătățire. În schimb, realizăm raționamentul **pe cazuri**:

```haskell
sumLength []
= {- proprietate sumLength -}
(sum [], length [])
= {- cazuri de bază sum și length -}
(0, 0)

sumLength (x : xs)
= {- proprietate sumLength -}
(sum (x : xs), length (x : xs))
= {- cazuri generale sum și length -}
(x + sum xs, 1 + length xs)
= {- izolare aplicații recursive -}
let (s, len) = (sum xs, length xs) in (x + s, 1 + len)
= {- proprietate sumLength citită invers -}
let (s, len) = sumLength xs in (x + s, 1 + len)
```

Astfel, obținem definiția mai eficientă, care parcurge lista **o singură dată**. Evident, media poate fi recuperată împărțind suma la lungime.

Să observăm că ambele funcții, `sum` și `length`, sunt compoziționale, și la fel este și ansamblul lor, `sumLength`. Acest lucru înseamnă că toate poate fi rescrise folosind `foldr`:

```haskell
sum = foldr (+) 0
length = foldr (\_ acc -> 1 + acc) 0 = foldr (const (+ 1)) 0
sumLength = foldr (\x (s, len) -> (x + s, 1 + len)) (0, 0)
```

Demersul de mai sus ridică două probleme importante:

- Aparent, prețul plătit pentru sporirea eficienței (o singură parcurgere în loc de două) este **renunțarea** la modularitate și reutilizare: nu mai refolosim independent `sum` și `length`, ci practic le reimplementăm monolitic în definiția lui `sumLength`.
- Pentru scrierea lui `sumLength` cu `foldr`, funcția binară și acumulatorul inițial se obțin prin **asamblarea** funcțiilor binare și ale acumulatorilor inițiali aferenți lui `sum` și `length`. Ar putea fi acest proces **abstractizat** pentru oricare două funcții scrise cu `foldr`?

În secțiunea următoare, descoperim că lucrurile nu stau așa sumbru, și că, deși  reutilizarea într-adevăr nu mai poate avea loc la nivelul funcțiilor `sum` și `length` înselor, ea poate rămâne totuși valabilă la alt nivel.

## Asamblarea reducerilor

Generalizând demersul de ER realizat asupra funcției `sumLength` în secțiunea anterioară, se obține următoarea proprietate, care permite rescrierea a **două** reduceri **independente** ca **una singură**.

```haskell
(foldr f a xs, foldr g b xs) = foldr (\x (acc1, acc2) -> (f x acc1, g x acc2)) (a, b) xs
```

Să vedem cum putem **automatiza** procesul de asamblare a funcțiilor binare și a acumulatorilor inițiali în vederea rescrierii cu un singur `foldr` în loc de două. În primul rând, să ne amintim originea tipurilor parametrilor aplicației `foldr f acc` pe liste. Acestea sunt obținute plecând de la tipurile **constructorilor de date** ai tipului `[a]`, și **înlocuind** aparițiile recursive ale tipului `[a]` însuși cu tipul `b` al acumulatorului:

```haskell
(:) :: a -> [a] -> [a]
=>
 f  :: a ->  b  ->  b

[]  :: [a]
=>
acc :: b
```

Astfel, putem defini un tip de date care încapsulează cei doi parametri: `Folder a b` include acumulatorul inițial și funcția binară drept câmpuri. Observați că tipurile lor corespund tipurilor primilor doi parametri ai lui `foldr`, dar în altă ordine; `a` este tipul elementelor listei, iar `b` este tipul acumulatorului. Numele `foldNull` și `foldCons` surprind corespondența cu cei doi constructori de date, `[]` și `(:)`, ai tipului `[a]`:

```haskell
data Folder a b = Folder
    { foldNull :: b
    , foldCons :: a -> b -> b
    }
```

Totuși, se dovedește că, în vederea **extinderii** acestui mecanism la reduceri **semi-dependente** (doar o reducere depinde de cealaltă) și **dependente** (fiecare reducere depinde de cealaltă), este mai avantajoasă o definiție mai flexibilă, în care tipul reducerii întregii liste (`c`) poate fi **diferit** de tipul reducerii restului listei (`b`):

```haskell
data Folder a b c = Folder
    { foldNull :: c
    , foldCons :: a -> b -> c
    }
```

Bineînțeles, reducerea clasică (`foldr`) poate fi recuperată doar în condițiile în care `b = c` (observați parametrul de tipul `Folder a b b`):

```haskell
fold :: Foldable t => Folder a b b -> t a -> b
fold folder = foldr (foldCons folder) (foldNull folder)
```

Constrângerea `Foldable t` este impusă de utilizarea lui `foldr` în implementarea lui `fold`. Prin urmare, deși discuția a fost purtată asupra listelor, mecanismul poate funcționa pentru **orice** instanță de `Foldable`.

De reținut că `foldr` surprinde într-adevăr transformări compoziționale, așa cum susține proprietatea de universalitate, dar **exact** transformările compoziționale pe vederea **liniară** asupra structurilor, **nu** orice transformare compozițională. Mai precis,

```haskell
foldr f acc struct = foldr f acc (toList struct)
```

unde primul `foldr` se realizează direct pe structura `Foldable`, iar al doilea, pe lista rezultată prin liniarizarea conținutului. Vom vedea mai jos cu putem surprinde orice transformări compoziționale și pe structuri **strict mai complexe** decât listele (de exemplu, arborii binari).

### Reduceri independente

Cum ne poate ajuta acest mecanism în cazul funcției `sumLength`? În primul rând, să rescriem corespunzător funcția `sum`:

```haskell
sum :: (Foldable t, Num a) => t a -> a
sum = fold sumFolder

sumFolder :: Num a => Folder a a a
sumFolder = Folder
    { foldNull = 0
    , foldCons = (+)
    }
```

Similar, rescriem funcția `length`:

```haskell
length :: Foldable t => t a -> Int
length = fold lengthFolder

lengthFolder :: Folder a Int Int
lengthFolder = Folder
    { foldNull = 0
    , foldCons = const (+ 1)
    }
```

Până în acest punct, doar am rescris funcțiile noastre utilizând noul mecanism. Avantajul este că, utilizând această reprezentare, putem introduce un operator care să **asambleze** două `Folder`-e **independente** într-unul singur:

```haskell
infixl 5 <+>
(<+>) :: Folder a b b
      -> Folder a c c
      -> Folder a (b, c) (b, c)
f <+> g = Folder
    { foldNull = (foldNull f, foldNull g)
    , foldCons = \a (b, c) -> (foldCons f a b, foldCons g a c)
    }
```

Operatorul primește două `Folder`-e care operează pe liste cu același tip `a` de elemente, dar care produc acumulatori diferiți, de tipurile `b`, respectiv `c`, și construiește un nou `Folder`, care produce un acumulator de tipul combinat `(b, c)`. În acest moment, putem defini automat un `Folder` pentru funcția `sumLength`:

```haskell
sumLength :: (Foldable t, Num a) => t a -> (a, Int)
sumLength = fold (sumFolder <+> lengthFolder)
```

Ce am obținut în final? Cu toate că nu am putut reutiliza funcțiile `sum` și `length` în sine, am putut **reutiliza** `sumFolder` și `lengthFolder`, păstrând **modularitatea** la acest nivel!

### Reduceri semidependente

Să analizăm acum exemplul funcției `steep`, care verifică dacă fiecare element al unei liste este mai mare ca suma elementelor din dreapta sa. O implementare directă arată astfel:

```haskell
steep :: (Ord a, Num a) => [a] -> Bool
steep [] = True
steep (x : xs) = x > sum xs && steep xs
```

Observăm următoarele:

- Având în vedere că `xs` contribuie la rezultat nu numai prin imaginea sa sub `steep`, ci și prin `sum`, funcția **nu** este compozițională și nu poate fi rescrisă imediat cu `foldr`.
- La fiecare pas se calculează suma restului listei, care conduce la o complexitate **pătratică**.

Dacă la fiecare pas am avea deja la dispoziție **suma** restului listei, complexitatea s-ar reduce la **liniară**. Prin urmare, aplicăm tehnica **îmbogățirii rezultatului** (*tupling*), transformând astfel funcția într-una **compozițională**, implementabilă ca o **reducere**.

Totuși, apare o **diferență** față de funcția `sumLength`, în care cele două reduceri constitutive, `sum`, respectiv `length`, sunt **independente**. În cazul funcției `steep`, transformarea principală **depinde unilateral** de `sum`, transformarea secundară. Numim cele două transformări ***semidependente***.

Din moment ce `sumFolder` este gata implementat, rămâne să implementăm `Folder`-ul aferent transformării principale. Aspectul esențial este că acesta citește un acumulator compus, cu tipul `(a, Bool)`, care include acumulatorul propriu de tipul `Bool`, dar și acumulatorul produs de `sumFolder`, de tipul `a`. Acumulatorul produs este doar cel propriu, de tipul `Bool`.

```haskell
steepFolder :: Ord a => Folder a (a, Bool) Bool
steepFolder = Folder
    { foldNull = True
    , foldCons = \x (s, stp) -> x > s && stp
    }
```

În continuare, introducem un nou operator de **asamblare** a `Folder`-elor **semidependente**:

```haskell
infixl 5 >.>
(>.>) :: Folder a b b
      -> Folder a (b, c) c
      -> Folder a (b, c) (b, c)
f >.> g = Folder
    { foldNull = (foldNull f, foldNull g)
    , foldCons = \a (b, c) -> (foldCons f a b, foldCons g a (b, c))
    }
```

Observați că primul `Folder` este **autonom**, operând cu acumulatori de tipul `b`. În schimb, al doilea `Folder` este **unidirecțional dependent** de primul, astfel încât pentru a produce un acumulator de tipul `c`, are nevoie nu numai de propriul său acumulator de tipul `c`, ci și de acumulatorul de tipul `b` produs de primul `Folder`. Parantezele unghiulare indică sensul transferului de informație dinspre primul spre al doilea `Folder`.

Acest operator este motivul pentru care am parametrizat constructorul de tip `Folder` cu trei parametri de tip în loc de doi; astfel, al doilea `Folder` poate citi un acumulator cu tipul `(b, c)`, **diferit** de tipul acumulatorului produs, `c`.

Cu acesta, noua implementare a funcției `steep` devine:

```haskell
steep :: (Ord a, Num a) => [a] -> (a, Bool)
steep = fold (sumFolder >.> steepFolder)
```

Dacă dorim întoarcerea doar a rezultatului boolean, puteam extrage a doua componentă a perechii produse:

```haskell
steep :: (Ord a, Num a) => [a] -> Bool
steep = snd . fold (sumFolder >.> steepFolder)
```

### Reduceri dependente

Ultimul exemplu este despre funcțiile `evenSum` și `oddSum`, care calculează suma elementelor de pe pozițiile pare, respectiv impare, dintr-o listă. O implementare posibilă utilizează **recursivitate mutuală**:

```haskell
evenSum :: Num a => [a] -> a
evenSum [] = 0
evenSum (x : xs) = x + oddSum xs

oddSum :: Num a => [a] -> a
oddSum [] = 0
oddSum (x : xs) = evenSum xs

evenOddSums xs = (evenSum xs, oddSum xs)
```

Observăm următoarele:

- Niciuna dintre funcții **nu** este compozițională.
- Dacă dorim să aflăm **ambele** sume simultan (funcția `evenOddSums`), lista este parcursă **de două ori**.

Dacă la fiecare pas am avea deja la dispoziție **cealaltă** sumă a restului listei, lista ar fi parcursă **o singură dată**. Prin urmare, aplicăm tehnica **îmbogățirii rezultatului** (*tupling*), transformând astfel funcția într-una **compozițională**, implementabilă ca o **reducere**.

Totuși, apare o **diferență** față de funcția `steep`, în sensul că acum cele două transformări depind **ambele** una de alta. Numim cele două transformări ***dependente***.

`Folder`-ele aferente celor două funcții urmează șablonul lui `steepFolder`, în sensul că citesc un acumulator compus, cu tipul `(a, a)`, care include atât acumulatorul propriu, cât și pe cel produs de cealaltă funcție. Fiecare `Folder` produce doar acumulatorul propriu.

```haskell
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
```

În continuare, introducem un nou operator de **asamblare** a `Folder`-elor **dependente**:

```haskell
infixl 5 <.>
(<.>) :: Folder a (b, c) b
      -> Folder a (b, c) c
      -> Folder a (b, c) (b, c)
f <.> g = Folder
    { foldNull = (foldNull f, foldNull g)
    , foldCons = \a (b, c) -> (foldCons f a (b, c), foldCons g a (b, c))
    }
```

Observați că ambele `Folder`-e citesc un acumulator de tipul `(b, c)`, **compus** din acumulatorii individuali, și produc doar acumulatorii **proprii** (`b`, respectiv `c`). Parantezele unghiulare indică dependențele **bidirecționale**.

Cu acestea, noua implementare a funcției `evenOddSums` devine:

```haskell
evenOddSums :: Num a => [a] -> (a, a)
evenOddSums = fold (evenSumFolder <.> oddSumFolder)
```

## Generalizarea reducerilor la alte structuri

În laboratorul anterior, am întâlnit clasa `Foldable`, care încearcă să generalizeze funcționala `foldr` și la alte structuri în afară de liste. De exemplu, în cazul tipului de arbore binar, cu definiția

```haskell
data BST a
    = BSTNod { vl :: a, lt :: BST a, rt :: BST a}
    | BSTNil
```

am putut instanția această clasă, și am putut defini funcțiile de liniarizare a cheilor (`contents`) și de calcul al dimensiunii (`sizeFold`) utilizând `foldr` pe arbori binari.

Am putea oare defini și funcția de calcul al înălțimii arborelui (`height`) utilizând `foldr`? Până la urmă, inspectând definiția sa,

```haskell
height :: BST a -> Int
height BSTNil = 0
height (BSTNod elem left right) = 1 + max (height left) (height right)
```

observăm că este **compozițională** (`left` și `right` contribuie doar prin imaginea lor sub `height`, iar `height` nu este aplicată pe alți parametri în afară de cei doi subarbori)!

Din păcate, dacă încercăm să o implementăm cu `foldr`, ne dăm seama că **nu reușim**. Care este problema? `foldr` expune o vedere **liniară** asupra arborelui, și, pe baza unui singur acumulator, **nu** putem distinge între informația provenită din subarborele stâng și cea provenită din subarborele drept, așa cum necesită calculul înălțimii (vedeți discuția de mai sus despre limitările lui `foldr`). Acest lucru înseamnă că, deși mecanismul bazat pe `Folder` și `fold` de mai sus **poate** fi aplicat și asupra arborilor, întrucât `BST` este instanță de `Foldable`, **nici el** nu ne poate ajuta în implementarea oricărei transformări compoziționale pe aceștia.

Din fericire, putem surprinde orice transformare compozițională pe arbori adaptând mecanismul bazat pe `Folder` și `fold` din secțiunea anterioară. Cum putem defini un `BSTFolder`? Răspunsul se obține oglindind în cazul arborilor procedeul de rescriere a tipurilor demonstrat mai sus asupra listelor. De data aceasta, trebuie să pornim de la tipurile **constructorilor de date** ai tipului `BST a` și să **înlocuim** aparițiile recursive ale tipului `BST a` însuși cu tipul `b` al acumulatorului:

```haskell
BSTNil  :: BST a
=>
foldNil :: b

BSTNod  :: a -> BST a -> BST a -> BST a
=>
foldNod :: a -> b     -> b     -> b
```

Prin urmare, am identificat definiția `BSTFolder`:

```haskell
data BSTFolder a b = BSTFolder
    { foldNil :: b
    , foldNod :: a -> b -> b -> b
    }
```

Ca și în cazul listelor, numele `foldNil` și `foldNod` reflectă numele celor doi constructori de date, `BSTNil` și `BSTNod`, ai tipului `BST a`. De asemenea, observăm că `foldNod` primește acum **doi acumulatori** de tipul `b`, câte unul aferent **fiecărui subarbore**, conducând la o **expresivitate superioară** lui `foldr`.

Dacă **flexibilizăm** tipul pentru a permite reduceri semidepdendente, analog `Folder`, obținem definiția finală:

```haskell
data BSTFolder a b c = BSTFolder
    { foldNil :: c
    , foldNod :: a -> b -> b -> c
    }
```

Ulterior, putem defini funcția propriu-zisă de reducere, capabilă să surprindă **orice** transformare **compozițională** pe arbori binari, având tipul:

```haskell
foldBST :: BSTFolder a b b -> BST a -> b
foldBST folder = go
  where
    go BSTNil = foldNil folder
    go (BSTNod elem left right) = foldNod folder elem (go left) (go right)
```

Cu aceste mecanisme, calculul înălțimii poate fi definit astfel:

```haskell
height :: BST a -> Int
height = foldBST heightFolder

heightFolder :: BSTFolder a Int Int
heightFolder = BSTFolder
    { foldNil = 0
    , foldNod = \_ leftHeight rightHeight -> 1 + max leftHeight rightHeight
    }
```

Din cele prezentate mai sus, pare necesară adaptarea explicită a mecanismului de reducere pentru fiecare nou tip de structură (listă, arbore binar etc.). Din fericire, este posibilă definirea în Haskell a unui mecanism universal, astfel încât funcțiile de reducere, ca `fold` și `foldBST`, precum și operatori ca `(<+>)`, `(>.>)` și `(<.>)` să aibă o definiție **unică**, independentă de structură! Nu vom dezvolta această idee, întrucât necesită mecanisme mai avansate de limbaj.

## Dualismul proprietate-definiție

În exemplele anterioare, am vehiculat **multiple proprietăți** îndeplinite de aceleași funcții. Totuși, din punct de vedere computațional, o funcție are într-un anumit program o **unică definiție**. O caracteristică fundamentală a programării funcționale este că definiția computațională **este** pur și simplu o colecție de una sau mai multe proprietăți satisfăcute de aceasta. Așa cum am constatat mai sus, alegerea definiției poate conduce la **eforturi de calcul** diferite.

Astfel, limbajele funcționale oferă o **notație comună** atât pentru scrierea **codului**, cât și pentru **raționarea** asupra acestuia.

## Resurse

- [Schelet](https://github.com/cs-pub-ro/PP-laboratoare/raw/refs/heads/archives25/archives/haskell-rationamente-schelet.zip) (exerciții în modulele `Fusion` și `BST`; exemple în modulul `List`)
- [Soluții](https://github.com/cs-pub-ro/PP-laboratoare/raw/refs/heads/archives25/archives/haskell-rationamente-solutii.zip)

## Referințe

- Bird, R., & Gibbons, J. (2020). *Algorithm Design with Haskell*. Cambridge: Cambridge University Press.
- Bird, R. (2014). *Thinking Functionally with Haskell*. Cambridge: Cambridge University Press.
