# Haskell: Introducere

-   Data publicării: x.04.2021
-   Data ultimei modificări: x.04.2021

## Obiective

Scopul acestui laborator este acomodarea cu primele noțiuni din Haskell
și observarea incipientă a diferențelor dintre acesta și Racket.

Aspectele urmărite sunt:

-   diferențele principale dintre cele 2 limbaje
-   definirea funcțiilor în Haskell
-   pattern matching
-   list comprehensions
-   tipuri
-   gărzi
-   funcționale
-   domenii de vizibilitate ale definițiilor: top-level și local
-   definirea de liste infinite
-   programare "point-free"

## Introducere

În jurul anilor 1990 un comitet de cercetători în limbaje de programare (Simon Marlow, Simon Peyton Jones, Philip Wadler etc) au creat un limbaj nou care a ajuns să fie standardul de-facto în cercetarea din domeniul programării funcționale. Inspirat dintr-o varietate de limbaje -- Miranda, ML, Scheme, APL, FP -- limbajul a influențat la rândul lui majoritatea limbajelor de programare cunoscute.

**Haskell** este un limbaj funcțional **pur**. Spre deosebire de limbajele imperative (sau Racket unde există *set!*), în Haskell aproape toate funcțiile sunt pure. Funcțiile impure sunt marcate diferit prin intermediul sistemului de tipuri.

În plus față de Racket, Haskell are **tipare statică** (și tipuri **polimorfice**). Fiecare expresie are un tip și este sarcina programatorului să efectueze conversiile necesare între tipuri dacă este necesar. De cele mai multe ori, informația despre ce face o funcție se găsește integral în tipul acesteia și numele ei. Astfel, se pune accentul pe *ce* face funcția, nu *cum* efectuează ea operațiile cerute.

Deși tipurile există, programatorul nu este obligat să depună efort în a le scrie în program. Haskell deține inferență de tipuri puternică. Exceptând cazurile în care se folosesc concepte avansate, programatorul poate lucra fără a scrie o singură semnătură de funcție (deși nu e recomandat pentru că se pierde o parte din documentația funcției).

În plus, tipurile ajută programatorul în procesul de scriere a codului transformându-l într-un exercițiu de rezolvare a unui puzzle: pur și simplu trebuiesc potrivite tipurile folosind funcții existente sau scriind alte funcții. Pentru a căuta funcțiile ce respectă o semnătură se poate folosi [Hoogle](http://www.haskell.org/hoogle/ "wikilink"), un motor de căutare similar Google dar doar pentru funcții Haskell.

Deosebirea fundamentală față de alte limbaje este **evaluarea leneșă**. Funcțiile nu vor fi evaluate și expresiile nu vor fi reduse până în momentul în care valoarea lor este necesară. Programatorul poate astfel lucra cu date infinite, extrăgând din ele strictul necesar pentru a obține soluția. Ca dezavantaj, analiza performanței unui cod Haskell este puțin mai dificilă, dar există instrumente auxiliare (dezvoltate în Haskell).

În cele ce urmează vom parcurge fiecare dintre aceste particularități,
accentuând diferențele față de Racket. Pe scurt, o **paralelă**
între cele două limbaje arată în felul urmator:

| **Limbaj** |  **Evaluare**  |  **Tipare**  | **Efecte laterale** |
|:-------:|:----------:|:--------:|:---------------:|
|  **Racket** | Aplicativă | Dinamică |        Da       |
| **Haskell** |   Leneșă   |  Statică |        Nu       |

Laboratoarele de Haskell din cadrul cursului se vor axa pe evidențierea acestor diferențe și a beneficiilor care rezultă din ele. Pentru noțiuni avansate despre limbaj, consultați primele 2 resurse din [bibliografie](#referințe "wikilink") (sau Reddit, Stack Overflow, Planet Haskell, etc.)

Un exemplu pentru diferența de evaluare dintre Racket și Haskell:
```lisp
(define (square x) (* x x))
```

```haskell 
square x = x * x
```

Evaluarea aplicativă din Racket va forța evaluarea parametrului x și apoi va apela funcția square: 
```lisp
(square (+ 1 2))
(square 3)
(* 3 3)
9
```

Evaluarea leneșă din Haskell va evalua parametrul x la cerere în cadrul apelului funcției square: 
```haskell
square (1 + 2)
(1 + 2) * (1 + 2)
3 * (1 + 2)
3 * 3
9
```

## GHC. GHCi

Există o varietate de compilatoare și interpretoare pentru Haskell. În momentul de față, limbajul și evoluția lui sunt strâns legate de eforturile dezvoltatorilor de la Glasgow. GHC, *The Glorious Glasgow Haskell Compilation System*, este și compilatorul pe care-l vom folosi pentru cursul de PP.

Pentru a avea o experiență bună cu acest limbaj, recomandarea este să vă instalați [Haskell
Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/ "wikilink") (citiți instrucțiunile de aici: [Limbaje](https://ocw.cs.pub.ro/courses/pp/21/limbaje "wikilink")). Față de compilator și de suita minimală de pachete, Haskell Stack aduce în plus o suită de biblioteci utile pentru dezvoltarea unor aplicații reale.

Codul Haskell poate fi atât compilat cât și interpretat. Pentru interpretare vom folosi **ghci**, iar pentru compilare vom folosi **ghc**. Fișierele de cod Haskell au în mod normal extensia `.hs`, dar se poate folosi și `.lhs` pentru variantele de **Literate Haskell** (programare ca o poveste - comentariile ocupă majoritatea textului în timp ce secvențele de cod sunt puține - este formatul preferat pentru publicarea de articole despre Haskell pe bloguri, oricine poate copia textul articolului într-un fișier și îl poate compila și rula apoi).

În cadrul laboratorului, recomandăm folosirea **ghci** în modul
următor:

-   se salvează definițiile de funcții într-un fișier cu extensia `.hs`
    (ex.: lab6-ex.hs)
-   se deschide consola sistemului (ex.: cmd/powershell pe Win) in
    directorul cu fisierul `.hs`
-   se rulează `stack exec ghci nume.hs` pentru a-l încărca în
    interpretor
-   se rulează în interpretor apelurile de funcții de test necesare,
    verificarea de tipuri, etc
-   dacă se dorește **editarea fișierului** se poate face într-un
    terminal separat (recomandat) sau folosind `:e` (`:edit`) din `ghci`
-   ATENTIE! după editarea fișierului, pentru a reîncărca definițiile se
    folosește `:re` (`:reload`)
-   dacă se dorește încărcarea altor module în interpretor (pentru
    testare), se poate folosi `:m +Nume.Modul`
-   pentru a verifica tipul unor expresii se folosește `:t expresie`
-   pentru a ieși din interpretor se folosește `:q` (sau EOF - `^Z` pe
    Linux/OSX, `^D` pe Windows)

Modulul `Prelude` conține funcții des folosite și este inclus implicit în orice fișier (deși poate fi exclus la nevoie, consultați [bibliografia](#referințe "wikilink")).

În cadrul unor programe reale va trebui să compilați sursele. Pentru aceasta va trebui să aveți un modul `Main.hs` care să conțină o funcție `main`. Modulul poate importa alte module prin `import Nume.Modul`. Pentru temă, nu va trebui să efectuați acești pași, scheletul de cod ce va fi oferit va conține partea de interacțiune cu mediul exterior. =)

## Comentarii

În Haskell, avem comentarii pe un singur rând, folosind `--` (2 de `-` legați) sau comentarii pe mai multe rânduri, încadrate de `{-` și `-}`. În plus, există comentarii pentru realizarea de documentație dar nu vom insista pe ele aici.

## Funcții

O funcție anonimă în **Racket**

```lisp 
(lambda (x y) (+ x y))
```

poate fi tradusă imediat în Haskell utilizând tot o *abstracție
lambda*

```haskell
\x y -> x + y
```

Sintaxa Haskell este ceva mai apropriată de cea a **calculului Lambda**: `\` anunță parametrii (separați prin spații), iar `->` precizează corpul funcției.

În Racket, puteam da nume funcției folosind legarea dinamică prin intermediul lui `define` sau legarea statică prin intermediul lui `let`. Exemplele următoare ilustrează cele 2 cazuri, împreună cu apelul funcției.

```lisp 
(define f (lambda (x y) (+ x y))) 
(f 2 3)
```
```lisp 
(let ((f (lambda (x y) (+ x y)))) 
  (f 2 3))
```

În Haskell, funcția echivalentă ar fi

```haskell
f = \x y -> x + y
```

sau

```haskell
f x y = x + y
```

Și apelul

```haskell
f 2 3
```

Observați că se renunță la paranteze. Unul dintre principiile dezvoltării limbajului a fost oferirea de construcții cât mai simple pentru lucrurile folosite cel mai des. Fiind vorba de programarea funcțională, aplicarea de funcții trebuia făcută cât mai simplu posibil.

Tot din același considerent, operatorii în Haskell sunt scriși în forma **infixată**. Restul apelurilor de funcții sunt în forma prefixată, exact ca în Racket. Totuși, se pot folosi ambele forme: pentru a folosi un operator prefixat se folosesc paranteze, în timp ce pentru a infixa o funcție se folosesc *back-quotes* (`).

```haskell
2 + 3 == (+) 2 3 
elem 2 [1,2,3] == 2 `elem` [1, 2, 3]
```

Tot pentru simplitate, Haskell permite folosirea unor secțiuni -- elemente de zăhărel sintactic care se vor traduce în funcții anonime:

```haskell
(2 +) == \x -> 2 + x
(+ 2) == \x -> x + 2
(- 2) == -2
(2 -) == \x -> 2 - x
```

**Atenție**: În construcția `(- x)` operatorul `-` este unar, nu binar (este echivalentul funcției `negate`). Dacă doriți să aplicați pațial la dreapta operatorul de scădere, utilizați funcția `subtract`, ca în expresia `(\`subtract\` 2)`.

## Tipuri de bază

În această secțiune vom prezenta tipurile existente în limbajul Haskell. Veți observa că limbajul este mult mai bogat în tipuri decât Racket. Programatorul își va putea defini alte tipuri proprii dacă dorește.

Pentru a putea vedea tipul unei expresii în `ghci` folosiți `:t expresie`.

```haskell
> :t 'a'
'a' :: Char
```

Operatorul `::` separă o expresie de tipul acesteia.

```haskell
> :t (42::Int)
(42::Int) :: Int
```

### Numere, caractere, siruri, booleeni

Următorii literali sunt valizi in **Haskell** (dupa operatorul `::` sunt precizate tipurile acestora):

```haskell
5 :: Int
'H' :: Char
"Hello" :: String -- sau [Char] -- lista de Char
True :: Bool
False :: Bool
```

Observați că există tipul caracter și tipul șir de caractere. Tipul `String` este de fapt un sinonim pentru tipul `[Char]` - tipul listă de caractere. Astfel, operațiile pe [liste](#liste "wikilink") vor funcționa și pe șiruri.

Tipurile numerice sunt puțin diferite față de alte limbaje de programare cunoscute:

```haskell
> :t 42
42 :: Num a => a
```

În exemplul de mai sus, `a` este o variabilă de tip (stă pentru orice fel de tip) restricționată (prin folosirea `=>`) la toate tipurile numerice (`Num a`).

Important de reținut este faptul că există **2** tipuri întregi: `Int` și `Integer`. Primul este finit, determinat de arhitectură, în timp ce al doilea este infinit, putând ajunge oricât de mare.

### Liste

O listă în Haskell se construiește similar ca în Racket, prin
intermediul celor doi constructori:

```haskell
[] -- lista vidă
(:) -- operatorul de adăugare la începutul listei - cons
```

Pentru simplitate, o construcție de forma

```haskell
1:3:5:[]
```

se poate scrie și

```haskell
[1, 3, 5]
```

sau

```haskell
[1, 3 .. 6]
```

sau

```haskell
[1, 3 .. 5]
```

Echivalente pentru `car` și `cdr` din Racket sunt funcțiile `head` și `tail`:

```haskell
> head [1, 2, 3]
1
> tail [1, 2, 3]
[2, 3]
```

Operatorul de **concatenare** este `++`:

```haskell
> [1, 2, 3] ++ [4, 5]
[1, 2, 3, 4, 5]
```

Haskell oferă un mod suplimentar de a genera liste: scriem proprietățile pe care ar trebui să le respecte elementele listei într-o sintaxă numită **list comprehension**. Este o sintaxă similară celei din matematică. De exemplu, vrem lista numerelor pare, divizibile cu 3. În matematică, am fi avut ceva de tipul `{x | x ∈ N2, x ≡ 0 (mod 3)}` (pentru `N2` mulțimea numerelor pare). În Haskell, avem

```haskell
> [x | x <- [0, 2 ..], x `mod` 3 == 0] -- lista numerelor naturale pare, divizibile cu 3
[0,6,12,18,24,30,36,42,48,54,60,66,72,78,84,90,96,102,108, Interrupted.
```

Observați că se generează elemente la infinit. Pentru fiecare element din lista `[0, 2 ..]` (din expresia `x <- [0, 2..]`) se testează condițiile următoare. Dacă toate sunt îndeplinite, se generează elementul din fața `|`.

Pentru a putea vedea o porțiune a fluxului folosim functiile `take` și `drop`.

### Perechi

În Haskell, elementele unei liste **sunt de același tip**. Dacă
dorim construcții cu elemente de tipuri diferite vom folosi tuplurile

```haskell
(3, "Ana") :: (Int, String)
(3, True, "Ana") :: (Int, Bool, String)
```

Haskell oferă funcții pentru extragerea componentelor din perechi

```haskell
> fst (3, "Ana")
3
> snd (3, "Ana")
"Ana"
```

Se ofera de asemenea și funcțiile `zip` și `unzip`, **doar** pentru perechi, dar există și `zip3` și `unzip3` pentru triplete.

Pentru tuplurile cu mai mult de 2 elemente este sarcina programatorului să definească funcțiile folosite.

## Definirea funcțiilor

Să considerăm că vrem să scriem o funcție pentru factorialul unui număr.
Prima implementare ar folosi sintaxa `if`:

```haskell
factorial_if x = if x < 1 then 1 else x * factorial_if (x - 1)
```

Este obligatoriu ca `if` să conțină **ambele** ramuri și ca acestea să fie **de același tip**.

Mai frumos, putem scrie funcția de mai sus folosind **gărzi**:

```haskell 
factorial_guards x
    | x < 1 = 1
    | otherwise = x * factorial_guards (x - 1)
```

Observați indentarea: orice linie care face parte din aceeași expresie ca cea de deasupra trebuie să înceapă la exact aceeași indentare. Orice linie care este o subexpresie a expresiei de mai sus (`then` sau `else` în cazul `if`, fiecare gardă în parte, etc.) trebuie să fie indentată mai spre dreapta.

Construcția `otherwise` este echivalentă expresiei `True`. O gardă poate conține orice fel de expresie care se va evalua la o valoarea de tip `Bool`.

Gărzile sunt testate în ordine, fiind evaluată expresia aferentă primei gărzi adevărate. Astfel, un set de gărzi este similar unei expresii `cond` din Racket, cu toate că există anumite restricții în privința locului în care pot fi definite gărzi.

Pentru a ne apropia de definiția matematică, putem scrie aceeași funcție folosind `case` (echivalentul `switch`-ului din C dar mult mai avansat):

```haskell 
factorial_case x = case x < 1 of
    True -> 1 
    _ -> x * factorial_case (x - 1)
```

Observați regula indentării aplicată și aici. Expresia `_` semnifică orice valoare, indiferent de valoarea ei. Expresia din `case` poate fi oricare.

Fiecare caz este tratat în ordine, prima potrivire este executată.

În final, putem scrie aceeași funcție folosind **pattern matching**:

```haskell
factorial_pm 0 = 1
factorial_pm x = x * factorial_pm (x - 1)
```

Expresiile din interiorul fiecărei ramuri din `case` sau din interiorul pattern-match nu pot fi decât constructorii unui tip (similar cu ce ați făcut la AA). În continuare vom folosi cele 4 stiluri pentru a ilustra funcția care calculează lungimea unei liste

```haskell
length_if l = if l /= [] then 1 + length_if (tail l) else 0
```
```haskell
length_guard l
    | l /= [] = 1 + length_guard (tail l)
    | otherwise = 0
```
```haskell 
length_case l = case l of
    (_ : xs) -> 1 + length_case xs
    _ -> 0
``` 
```haskell
length_pm [] = 0
length_pm (_:xs) = 1 + length_pm xs
```

Observați în exemplele de mai sus expresivitatea limbajului. Folosirea
construcției potrivite duce la definiții scurte și ușor de înțeles.

## Curry vs Uncurry

Fiind un limbaj funcțional, în Haskell funcțiile sunt valori de prim
rang. Astfel, putem afla tipul unei funcții:

```haskell
f x y = x + y
```

```haskell
> :t f
f :: Num a => a -> a -> a
```

Fiecare argument este separat prin `->` de următorul sau de rezultat.

Amintindu-ne de discuția despre [funcții curry și
uncurry](../racket/functionale#functii-curryuncurry "wikilink"),
rezultatul următor nu trebuie să ne surprindă

```haskell
> :t f 3
f 3 :: Num a => a -> a
```

Toate funcțiile din Haskell sunt în formă **curry**.

Pentru a face o funcție uncurry putem folosi funcția `uncurry :: (a -> b -> c) -> (a, b) -> c` dacă vrem să transformăm o funcție echivalentă sau putem s-o definim în mod direct folosind perechi.

## Funcționale uzuale

Haskell oferă un set de funcționale (similar celui din Racket) pentru cazuri de folosire des întâlnite (dacă nu mai știți ce fac, încercați să ghiciți citind semnătura și numele ca o documentație):

```haskell
> :t map
map :: (a -> b) -> [a] -> [b]
> :t filter
filter :: (a -> Bool) -> [a] -> [a]
> :t foldl
foldl :: (a -> b -> a) -> a -> [b] -> a
> :t foldr
foldr :: (a -> b -> b) -> b -> [a] -> b
> :t zip
zip :: [a] -> [b] -> [(a, b)]
```

Folosirea lor duce la un cod mai ușor de citit și de întreținut.

## Domenii de vizibilitate

Spre deosebire de [Racket](https://ocw.cs.pub.ro/courses/pp/21/laboratoare/racket/legare "wikilink"), unde legarea variabilelor la nivelul cel mai de sus (top-level) este dinamică, Haskell leagă definițiile **static**, acestea fiind vizibile implicit la nivel **global**. De exemplu, o definiție de forma:

```haskell
theAnswer = 42
```

va putea fi utilizată implicit în toate fișierele încărcate de către compilator/interpretor la un moment dat. Domeniul de vizibilitate al definițiilor top-level poate fi însă redus cu ajutorul definirii de module: consultați capitolul 11 din [A Gentle Introduction to Haskell](http://www.haskell.org/tutorial/modules.html "wikilink") pentru mai multe detalii.

La fel ca Racket, Haskell permite definirea în cadrul domeniilor de
vizibilitate locală (mai exact în cadrul funcțiilor), cu ajutorul
clauzelor `let` și `where`.

### let

Forma generală a clauzei `let` este următoarea:

```haskell
let id1 = val1
    id2 = val2
    ...
    idn = valn
in expr
```

unde `expr` este o expresie Haskell care poate depinde de `id1, id2, ..., idn`. De asemenea, domeniul de vizibilitate ale definițiilor locale este întreaga clauză `let` (similar cu `letrec` în Racket). Astfel, definiția următoare:

```haskell
p = let x = y + 1
        y = 2
        b n = if n == 0 then [] else n : b (n - 1)  
    in (x + y, b 2)
```

este corectă. `x` poate să depindă de `y` datorită **evaluării leneșe**: în fapt `x` va fi evaluat în corpul clauzei, în cadrul expresiei `(x + y, b 2)`, unde `y` e deja definit.

### where

Clauza `where` este similară cu `let`, diferența principală constând în folosirea acesteia **după** corpul funcției. Forma generală a acesteia este:

```haskell 
def = expr
    where  
    id1 = val1  
    id2 = val2  
    ...
    idn = valn
```
cu aceleași observații ca în cazul `let`.

Un exemplu de folosire este implementarea metodei de sortare QuickSort:

```haskell
qsort [] = [] 
qsort (p : xs) = qsort left ++ [p] ++ qsort right
    where  
    left = filter (< p) xs
    right = filter (>= p) xs
```

Clauzele de tip `let` și `where` facilitează **reutilizarea** codului. De exemplu, funcția:

```haskell
inRange :: Double -> Double -> String 
inRange x max
    | f < low               = "Too low!"  
    | f >= low && f <= high = "In range"  
    | otherwise             = "Too high!"  
    where
    f = x / max
    (low, high) = (0.5, 1.0)
```

verifică dacă o valoare normată se află într-un interval fixat. Expresia dată de `f` este folosită de mai multe ori în corpul funcției, motiv pentru care este urmărită încapsularea ei într-o definiție. De asemenea, se observă că definițiile locale, ca și cele top-level, permit pattern matching-ul pe constructorii de tip, în cazul acesta constructorul tipului pereche.

Observăm că `where` și `let` sunt de asemenea utile pentru definirea de funcții auxiliare:

```haskell
naturals = iter 0
    where iter x = x : iter (x + 1)
```

`iter` având în exemplul de mai sus rolul de generator auxiliar al listei numerelor naturale.

## Liste infinite și generarea lor folosind funcționale

Haskell facilitează generarea de liste infinite, dată fiind natura **leneșă** a evaluării expresiilor. De exemplu mulțimea numerelor naturale poate fi definită după cum urmează:

```haskell
naturals = [0..]
```

Definiția de mai sus este însă **zahăr sintactic** pentru următoarea expresie (exemplificată anterior):

```haskell 
naturals = iter 0
    where iter x = x : iter (x + 1)
```

Putem de asemenea să generăm liste infinite folosind funcționala [iterate](http://hackage.haskell.org/package/base-4.6.0.1/docs/Prelude.html#v:iterate "wikilink"), având prototipul:

```haskell
> :t iterate
iterate :: (a -> a) -> a -> [a]
```

`iterate` primește o funcție `f` și o valoare inițială `x` și generează o listă infinită din aplicarea repetată a lui `f`. Implementarea listei numerelor naturale va arăta deci astfel:

```haskell
naturals = iterate (\ x -> x + 1) 0
```

Observăm că `iterate` este nu numai o funcțională, ci și un **șablon de proiectare** (design pattern). Alte funcționale cu care putem genera liste infinite sunt:

-   `repeat`: repetă o valoare la infinit
-   `intersperse`: introduce o valoare între elementele unei liste
-   `zipWith`: generalizare a lui `zip` care, aplică o funcție binară pe elementele a două liste; e completată de `zipWith3` (pentru funcții ternare), `zipWith4` etc.
-   `foldl`, `foldr`, `map`, `filter`

Exemple de utilizare:

```haskell 
ones = repeat 1 -- [1, 1, 1, ..] 
onesTwos = intersperse 2 ones -- [1, 2, 1, 2, ..] 
fibs = 1 : 1 : zipWith (+) fibs (tail fibs) -- sirul lui Fibonacci 
powsOfTwo = iterate (\* 2) 1 -- puterile lui 2
palindromes = filter isPalindrome [0..] -- palindroame
    where  
    isPalindrome x = show x == reverse (show x) -- truc: reprezint numarul ca String
```

## Point-free programming

În Haskell, compunerea funcțiilor se realizează cu ajutorul operatorului `.`. De interes este și operatorul `$` definit ca:

```haskell
f $ x = f x
```

El are avantajul de a grupa expresiile din dreapta și stânga lui înainte de aplicarea funcției, scăpând astfel de paranteze:

```haskell
length $ 3 : [1, 2] -- length (3 : [1, 2])
```

Împreună cu `curry`, `uncurry` și `flip` aceste funcții duc la un stil de programare în care valoarea în care se evaluează funcția nu este prezentă. Urmăriți exemplul următor de transformare

```haskell
square x = x*x
inc x = x+1
f1 x = inc (square x)
f2 x = inc $ square x
f3 x = inc . square $ x
f4 = inc . square
```

Stilul are câteva avantaje în domeniul expresivității și al verificării programului dar poate duce ușor la cod obfuscat.

["Point-free style"](https://wiki.haskell.org/Pointfree "wikilink") reprezintă o paradigmă de programare în care evităm menționarea explicită a parametrilor unei funcții în definiția acesteia. Cu alte cuvinte, se referă la scrierea unei funcții ca o succesiune de compuneri de funcții. Această abordare ne ajută, atunci când scriem sau citim cod, să ne concentrăm asupra obiectivului urmărit de algoritm deoarece expunem mai transparent ordinea în care sunt efectuate operațiile. În multe situații, codul realizat astfel este mai compact și mai ușor de urmărit.

De exemplu, putem să definim operația de însumare a elementelor unei liste in felul următor:

```haskell
> let sum xs = foldl (+) 0 xs
```

Alternativ, în stilul "point-free", vom evita descrierea explicită a argumentului funcției:

```haskell
> let sum = foldl (+) 0
```

Practic, ne-am folosit de faptul că în Haskell toate funcțiile sunt în formă [curry](https://wiki.haskell.org/Currying "wikilink"), aplicând parțial funcția `foldl` pe primele doua argumente, pentru a obține o expresie care așteaptă o listă și produce rezultatul dorit.

Pentru a compune două funcții, vom folosi operatorul `.`:

`> :t (.)` `(.) :: (b -> c) -> (a -> b) -> a -> c`

Dacă analizăm tipul operatorului, observăm că acesta primește ca argumente o funcție care acceptă o intrare de tipul `b` și întoarce o valoare de tipul `c`, respectiv încă o funcție care primește o intrare de tipul `a` și produce o valoare de tipul `b`, compatibilă cu intrarea așteptată de prima funcție. Rezultatul întors este o funcție care acceptă valori de tipul `a` și produce rezultate de tipul `c`.

Cu alte cuvinte, expresia `(f . g)(x)` este echivalentă cu `f(g(x))`.

Spre exemplu, pentru a calcula expresia `2 * x + 1` pentru orice element dintr-o listă, fără a folosi o funcție lambda, putem scrie direct:

```haskell
> let sm = map ((+ 1) . (* 2))
> :t sm
sm :: [Integer] -> [Integer]
```

De observat că au fost necesare paranteze pentru ca întreaga expresie `(+ 1) . (* 2)` să reprezinte un singur argument pentru funcția `map`. Am putea obține un cod mai concis folosind operatorul `$`:

```haskell
> let sm = map $ (+ 1) . (* 2)
> :t ($)
($) :: (a -> b) -> a -> b
```

`$` este un operator care aplică o funcție unară pe parametrul său. Semantica expresiei `f $ x` este aceeași cu cea a `f x`, diferența fiind pur sintactică: precedența `$` impune ordinea de evaluare astfel încât expresia din stânga `$` este aplicată pe cea din dreapta. Avantajul practic este că putem sa omitem parantezele în anumite situații, în general atunci când ar trebui să plasăm o paranteză de la punctul unde se află `$` până la sfârșitul expresiei curente.

De exemplu, expresiile următoare sunt echivalente:

```haskell
> length (tail (zip [1,2,3,4] ("abc" ++ "d")))
> length $ tail $ zip [1,2,3,4] $ "abc" ++ "d"
```

Alternativ, folosind operatorul de compunere `.`:

```haskell
> (length . tail . zip [1,2,3,4]) ("abc" ++ "d")
> length . tail . zip [1,2,3,4] $ "abc" ++ "d"
```

**Atenție!**: Cei doi operatori, `.` și `$` nu sunt, în general, interschimbabili:

```haskell
> let f = (+ 1)
> let g = (* 2)
> :t f . g
f . g :: Integer -> Integer
> :t f $ g
-- eroare, f așteaptă un Integer, g este o funcție
> f . g $ 2
5
> f . g 2
-- eroare, echivalent cu f . (g 2), f . g (2)
> f $ g $ 2
5
> f $ g 2
5
```

Uneori, ne dorim să schimbăm ordinea de aplicare a argumentelor pentru o funcție. Un exemplu de funcție care ne-ar putea ajuta în acest sens este funcția `flip`, care interschimbă parametrii unei funcții binare:

```haskell
flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x
```

De exemplu, dacă vrem să construim o funcție point-free care aplică o funcție primită ca argument pe fluxul numerelor naturale, am putea scrie:

```haskell
> :t map
map :: (a -> b) -> [a] -> [b]
> :t flip map
flip map :: [a] -> (a -> b) -> [b]
> let f = flip map $ [0..]
> take 10 $ f (*2)
[0,2,4,6,8,10,12,14,16,18]
```

Un șablon de proiectare des folosit este `concatMap`, sau [map/reduce](https://en.wikipedia.org/wiki/MapReduce "wikilink"), care implică compunerea funcționalelor `map` și `fold`. De exemplu, funcția `intersperse`, prezentată anterior, poate fi definită sub forma:

```haskell
myIntersperse :: a -> [a] -> [a]
myIntersperse y = foldr (++) [] . map (: [y])
```

Observăm că al doilea argument al funcției `myIntersperse` nu este menționat explicit.

Atenție! Folosirea stilului "point-free" poate să scadă în anumite cazuri lizibilitatea codului! Ideal este să folosiți construcții "point-free" doar atunci când acestea fac programul **mai ușor** de înțeles. Proiectele Haskell de dimensiuni mari încurajează stilul "point-free" **doar** împreună cu clauze `let` și `where`, și uneori cu funcții anonime.

Un exemplu de aplicație uzuală de "point-free style programming", cu care probabil sunteți deja familiari, este folosirea operatorului pipe ("|") în [unix shell scripting](http://www.vex.net/~trebla/weblog/pointfree.html "wikilink").

## Traduceri cod din Racket în Haskell
Aici, vom face comparații între Racket și Haskell, în ceea ce privește sintaxa, pe categorii.

Funcții lambda:
- Racket
```lisp
(lambda (x) (+ x 1))
```
- Haskell
```haskell
\x -> x + 1
```

Operatori aritmetici:
- Racket
```lisp
(+ 1 2)
(- 7 2)
(* 2 11)
(/ 5 2)
(quotient 5 2)
(modulo 5 2)
```
- Haskell
```haskell
1 + 2
7 - 2
2 * 11
5 / 2
mod 7 2
quot 7 2
```

Operatori logici:
- Racket
```list
(not #t) ; not
(not #f) ; not
(or #t #f) ; or
(and #t #f) ; and
```
- Haskell
```haskell
not True -- not
not False -- not
True || False -- or
True && False -- and
```

Apelare de funcții:
- Racket
```lisp
(max 2 3)
```
- Haskell 
```haskell
max 2 3
```

Perechi:
- Racket
```lisp
(cons 1 2) ; construirea unei perechi
(car (cons 1 2)) ; primul element
(cdr (cons 1 2)) ; al doilea element
```
- Haskell - sunt tupluri, mai precis colecții care pot avea elemente de tipuri diferite
```haskell
(1, 2) -- construirea unei perechi
(fst (1, 2)) -- primul element
(snd (1, 2)) -- al doilea element

(1, 2, 3, 4, [], "aaaa") -- un tuplu care are mai multe elemente, de tipuri diferite
```

Liste:
- Racket - listele sunt eterogene, pot conține elemente de tipuri diferite
```lisp
null ; lista goala
'() ; lista goala

(list 1 2 3 4) ; o lista de numere

(car (list 1 2 3 4)) ; primul element din lista
(cdr (list 1 2 3 4)) ; restul listei, fara primul element

(cons 0 (list 1 2 3 4)) ; adaugarea unui element la inceputul unei liste

(null? (list 1 2 3 4)) ; se verifica daca lista este goala

(length (list 1 2 3 4)) ; lungimea unei liste

(append (list 1 2 3 4) (list 5 6 7)) ; concatenarea dintre doua liste

(member 4 (list 1 2 3 4)) ; se verifica daca un element exista intr-o lista
```
- Haskell - listele sunt omogene, au elemente de același tip
```haskell
[] -- lista goala

[1, 2, 3, 4] -- o lista de numere

head [1, 2, 3, 4] -- primul element din lista
tail [1, 2, 3, 4] -- restul listei, fara primul element

0 : [1, 2, 3, 4] -- adaugarea unui element la inceputul unei liste

null [1, 2, 3, 4] -- se verifica daca lista este goala
[] ==  [1, 2, 3, 4] -- se verifica daca lista este goala (trebuie sa elementele sa poata fi comparate - vom discuta despre Eq la laboratorul de clase in Haskell)

length [1, 2, 3, 4] -- lungimea unei liste

[1, 2, 3, 4] ++ [5, 6, 7] -- concatenarea dintre doua liste

elem 4 [1, 2, 3, 4] -- se verifica daca un element exista intr-o lista
4 `elem` [1, 2, 3, 4] -- se verifica daca un element exista intr-o lista
```

Sintaxa if:
- Racket
```lisp
(if (< a 0)
    (if (> a 10) 
        (*a a) 
            0)
    -1))
```
- Haskell
```haskell
if a < 0 then 
    if (a > 10) 
        then a * a 
        else 0
    else -1
```

Definirea unei funcții:
- Racket
```lisp
(define (sum-list l)
  (if (null? l)
      0
      (+ (car l) (sum-list (cdr l)))))
```
- Haskell
```haskell
-- cu if-else-then
sumList :: [Int] -> Int
sumList l = if null l then 0 else head l + sumList (tail l)

-- pattern matching
sumList2 :: [Int] -> Int
sumList2 [] = 0
sumList2 (x:xl) = x + sumList2 xl

-- cu garzi
sumList3 :: [Int] -> Int
sumList3 l
    | null l = 0
    | otherwise = head l + sumList3 (tail l)

-- cu case of
sumList4 :: [Int] -> Int
sumList4 l = case l of
    [] -> 0
    (x:xl) -> x + sumList4 xl
```

Funcționale:
- Racket
```lisp
; map
(map (λ (x) (+ x 1)) (list 1 2 3 4))
(map add1 (list 1 2 3 4))

; filter
(filter (λ (x) (equal? (modulo x 2) 0)) (list 1 2 3 4 5 6))

; foldl
(reverse (foldl (lambda (x acc) (cons x acc)) '() (list 1 2 3 4 5)))

; foldr
(foldr (lambda (x acc) (cons x acc)) '() (list 1 2 3 4 5))
```
- Haskell
```haskell
-- map
map (\x -> x + 1) [1, 2, 3, 4]
map (+ 1) [1, 2, 3, 4]

-- filter
filter (\x -> mod x 2 == 0) [1, 2, 3, 3, 4, 5, 6]

-- foldl
reverse $ foldl (\acc x -> x : acc) [] [1, 2, 3, 4, 5] -- [1, 2, 3, 4, 5]

-- foldr
foldr (\x acc -> x : acc) [] [1, 2, 3, 4, 5] -- [1, 2, 3, 4, 5]
```

Legări:
- Racket
- Haskell - `let` în Haskell se comporta precum `letrec` din Racket
```haskell
-- cu let
f a = 
    let c = a
        b = a + 1
    in (c + b) -- let din Racket

g a = 
    let c = a
        b = c + 1
    in (c + b) -- let* din Racket

h a = 
    let c = b
        b = a + 1
    in (c + b) -- letrec din Racket, aici nu avem eroare datorita evaluarii lenese

-- cu where
f' a = (c + b)
    where
        c = a
        b = a + 1  -- let din Racket

g' a = (c + b)
    where
        c = a
        b = c + 1  -- let* din Racket

h' a = (c + b)
    where
        c = b
        b = a + 1  -- letrec din Racket, aici nu avem eroare datorita evaluarii lenese
```
## Resurse

-   {{ :20:laboratoare:haskell:intro-ex.zip \| Exerciții}}
-   {{ :20:laboratoare:haskell:haskell-cheatsheet-1.pdf \| Cheatsheet}}
-   {{ :20:laboratoare:haskell:intro-sol.zip \| Soluții}}

## Referințe

-   *[Learn You a
    Haskell](http://learnyouahaskell.com/chapters "wikilink")*, este
    utilă pentru toate laboratoarele de Haskell plus ceva extra
-   *[Hoogle](http://www.haskell.org/hoogle/ "wikilink")*, search for
    Haskell functions starting from types
-   *[Hackage](http://hackage.haskell.org/packages/hackage.html "wikilink")*,
    repository pentru pachete Haskell
-   *[Pointfree on Haskell
    Wiki](http://www.haskell.org/haskellwiki/Pointfree "wikilink")*
-   *[Pointfree Style - Whay is it good
    for](http://buffered.io/posts/point-free-style-what-is-it-good-for "wikilink")*
-   *[Advantages of point-free on Lambda the
    Ultimate](http://lambda-the-ultimate.org/node/3233 "wikilink")*
-   *[Local definitions](http://en.wikibooks.org/wiki/Haskell/Variables_and_functions#Local_definitions "wikilink")*
-   *[Comparație](http://www.lambdadays.org/static/upload/media/14254629275479beameraghfuneval2.pdf "wikilink")* între diferiți algoritmi de sortare implementați in limbaje funcționale
-   *[Let vs Where](https://wiki.haskell.org/Let_vs._Where "wikilink")*
-   *[ Haskell: function composition (.) vs function application ($) (SO)](http://stackoverflow.com/questions/3030675/haskell-function-composition-and-function-application-idioms-correct-us "wikilink")*
-   *[Pointfree (Haskell Wiki)](http://www.haskell.org/haskellwiki/Pointfree "wikilink")*
-   *[Higher order function (Haskell Wiki)](http://www.haskell.org/haskellwiki/Higher_order_function "wikilink")*
