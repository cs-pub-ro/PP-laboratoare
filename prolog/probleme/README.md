# Prolog: Probleme

-   Data publicării: 23.05.2021
-   Data ultimei modificări: 23.05.2021

## Obiective

Scopul acestui laborator îl reprezintă învățarea unor concepte avansate de programare în Prolog, legate de obținerea tuturor soluțiilor ce satisfac un scop.

## Aflarea tuturor soluțiilor pentru satisfacerea unui scop

Prolog oferă un set special de predicate care pot construi liste din toate soluțiile de satisfacere a unui scop. Acestea sunt extrem de utile deoarece altfel este complicată culegerea informațiilor la revenirea din backtracking (o alternativă este prezentată în secțiunea următoare).

### findall(+Template, +Goal, -Bag)

Predicatul `findall` pune în `Bag` câte un element Template pentru fiecare soluție a expresiei `Goal`. Desigur, predicatul este util atunci când `Goal` și `Template` au variabile comune. De exemplu: 
```prolog
even(Numbers, Even):-
    findall(X,(member(X, Numbers), X mod 2 =:= 0), Even).

?- even([1, 2, 3, 4, 5, 6, 7, 8, 9], Even). Even = [2, 4, 6, 8].
```

### bagof(+Template, +Goal, -Bag)

Predicatul `bagof` seamănă cu `findall`, diferența fiind că `bagof` construiește câte o listă `Bag` pentru fiecare instanțiere diferită a variabilelor libere (nelegate) din `Goal` care nu se găsesc în `Template`.

```prolog 
digits([1, 2, 3, 4, 5, 6, 7, 8, 9]).
numbers([4, 7, 9, 14, 15, 18, 24, 28, 33, 35]).

multiples(D,L):-
    digits(Digits),  
    numbers(Numbers),  
    bagof(N,(member(D, Digits), member(N, Numbers), N mod D =:= 0), L).

?- multiples(D,L).
D = 1, L = [4, 7, 9, 14, 15, 18, 24, 28, 33|...] ;
D = 2, L = [4, 14, 18, 24, 28] ;
D = 3, L = [9, 15, 18, 24, 33] ;
D = 4, L = [4, 24, 28] ;
D = 5, L = [15, 35] ;
D = 6, L = [18, 24] ;
D = 7, L = [7, 14, 28, 35] ;
D = 8, L = [24] ;
D = 9, L = [9, 18].
```

Aici, variabilele `D` și `N` din `Goal` nu sunt legate, dar `N` apare în `Template`, astfel că rezultatele (soluțiile pentru `L`) vor fi separate după posibilele legări pentru `D`.
Pentru a evita *gruparea* soluțiilor pentru fiecare valoare separată a variabilelor ce apar în scopul lui `bagof/3` se poate folosi construcția `Var^Goal`.

Exemple:
```prolog
are(andrei, laptop, 1). 
are(andrei, pix, 5). 
are(andrei, ghiozdan, 2).
are(radu, papagal, 1). 
are(radu, ghiozdan, 1). 
are(radu, laptop, 2).
are(ana, telefon, 3). 
are(ana, masina, 1).

?- bagof(X, are(andrei, X, _), Bag).
Bag = [laptop] ;
Bag = [ghiozdan] ;
Bag = [pix].

?- bagof(X, C^are(C, X, _), Bag).
Bag = [laptop, papagal, ghiozdan, masina] ;
Bag = [ghiozdan, laptop] ;
Bag = [telefon] ;
Bag = [pix].

?- bagof(X, P^C^are(C, X, P), Bag).
Bag = [laptop, pix, ghiozdan, papagal, ghiozdan, laptop, telefon, masina].

?- bagof(X, are(C, X, _), Bag).
C = ana,
Bag = [masina] ;
C = ana,
Bag = [telefon] ;
C = andrei,
Bag = [laptop] ;
C = andrei,
Bag = [ghiozdan] ;
C = andrei,
Bag = [pix] ;
C = radu,
Bag = [papagal, ghiozdan] ;
C = radu,
Bag = [laptop].

?- bagof(X, are(C, X, P), Bag).
C = ana,
P = 1,
Bag = [masina] ;
C = ana,
P = 3,
Bag = [telefon] ;
C = andrei,
P = 1,
Bag = [laptop] ;
C = andrei,
P = 2,
Bag = [ghiozdan] ;
C = andrei,
P = 5,
Bag = [pix] ;
C = radu,
P = 1,
Bag = [papagal, ghiozdan] ;
C = radu,
P = 2,
Bag = [laptop].

?- bagof(X, P^are(C, X, P), Bag).
C = ana,
Bag = [telefon, masina] ;
C = andrei,
Bag = [laptop, pix, ghiozdan] ;
C = radu,
Bag = [papagal, ghiozdan, laptop].
```

### setof(+Template, +Goal, -Bag)

Predicatul `setof/3` are aceeași comportare cu `bagof/3`, dar cu diferența că soluțiile găsite sunt sortate și se elimină duplicatele.

Exemple:
```prolog
?- setof(X, are(andrei, X, _), Bag).
Bag = [laptop] ;
Bag = [ghiozdan] ;
Bag = [pix].

?- setof(X, C^are(C, X, _), Bag).
Bag = [ghiozdan, laptop, masina, papagal] ;
Bag = [ghiozdan, laptop] ;
Bag = [telefon] ;
Bag = [pix].

?- setof(X, P^C^are(C, X, P), Bag).
Bag = [ghiozdan, laptop, masina, papagal, pix, telefon].

?- setof(X, are(C, X, _), Bag).
C = ana,
Bag = [masina] ;
C = ana,
Bag = [telefon] ;
C = andrei,
Bag = [laptop] ;
C = andrei,
Bag = [ghiozdan] ;
C = andrei,
Bag = [pix] ;
C = radu,
Bag = [ghiozdan, papagal] ;
C = radu,
Bag = [laptop].

?- setof(X, are(C, X, P), Bag).
C = ana,
P = 1,
Bag = [masina] ;
C = ana,
P = 3,
Bag = [telefon] ;
C = andrei,
P = 1,
Bag = [laptop] ;
C = andrei,
P = 2,
Bag = [ghiozdan] ;
C = andrei,
P = 5,
Bag = [pix] ;
C = radu,
P = 1,
Bag = [ghiozdan, papagal] ;
C = radu,
P = 2,
Bag = [laptop].

?- setof(X, P^are(C, X, P), Bag).
C = ana,
Bag = [masina, telefon] ;
C = andrei,
Bag = [ghiozdan, laptop, pix] ;
C = radu,
Bag = [ghiozdan, laptop, papagal].
```

### forall(+Cond, +Action)
Predicatul `forall/2` verifică dacă pentru orice legare din `Cond`, care reprezintă un domeniu ce conține legări de variabile, se pot îndeplini condițiile din `Action`.

Exemple:
```prolog
?- forall(member(X,[2, 4, 6]), X mod 2 =:= 0).
true.

?- forall(member(X,[2, 4, 3, 6]), X mod 2 =:= 0).
false.

?- forall(member(X, [6, 12, 18]), (X mod 2 =:= 0, X mod 3 =:= 0)).
true.
```

## Câteva observații asupra purității

În logica de ordinul întâi clauzele `p(A,B) ^ q(A,B)` și `q(A,B) ^ p(A,B)` sunt echivalente. Ordinea termenilor dintr-o conjuncție (sau disjuncție) nu influențează valoarea de adevăr a clauzei.

În Prolog acest lucru nu este întotdeauna adevărat:

```prolog
?- X = Y, X == Y. 
X = Y.

?- X == Y, X = Y. 
false.
```

## Resurse
- [Cheatsheet](https://github.com/cs-pub-ro/PP-laboratoare/blob/master/prolog/probleme/prolog_cheatsheet_3.pdf)
- [Schelet](https://ocw.cs.pub.ro/courses/_media/pp/21/laboratoare/prolog/probleme-skel.zip)
- [Soluții](https://ocw.cs.pub.ro/courses/_media/pp/21/laboratoare/prolog/probleme-sol.zip)
