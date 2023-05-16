#  Prolog: Legare și execuție

* Data publicării: 09.05.2023
* Data ultimei modificări: 09.05.2023

## Obiective

Scopul acestui laborator este introducerea și înțelegerea unor noțiuni mai
avansate de Prolog:

* procesul de backtracking realizat de Prolog
* lucrul cu variabile neinstanțiate (nelegate)
* controlul execuției și predicatul cut.
* obținerea tuturor soluțiilor ce satisfac un scop.

## Recapitulare

Laboratorul trecut a introdus două elemente ale limbajului importante:

- [variabile](https://www.swi-prolog.org/pldoc/man?section=glossary#gloss:variable)
  > O variabilă este o valoare care nu a fost încă legată
- operatorul de
  [unificare](https://www.swi-prolog.org/pldoc/man?section=glossary#gloss:unify),
  `=` [doc](https://www.swi-prolog.org/pldoc/doc_for?object=(%3D)/2)

Un exemplu foarte simplu de legare unei variabile este prima interogare de mai
jos, unde `X` s-a legat la atomul (constanta) `socrate` pentru a satisface
scopul `om(X)` și apoi este printat atomul.

```prolog
om(socrate).

caesar(gaiusIulius).      % Divus Iulius - divinul Iulius
caesar(octavianAugustus). % Divi filius  - fiu divin

?- om(X), writeln(X).
socrate
X = socrate. % legarea făcută pe parcursul satisfacerii scopului

?- om(X), caesar(X), writeln(X).
false.
```

În a doua interogare nicio regulă (cu corp sau nu) a predicatului `caesar` nu
permite unificarea cu expresia `caesar(socrate)`. Practic:

```prolog
[trace]  ?- om(X), caesar(X), writeln("Succes").
   Call: (11) om(_18822) ? creep
   Exit: (11) om(socrate) ? creep
   Call: (11) caesar(socrate) ? creep
   Fail: (11) caesar(socrate) ? creep
false.
```

La al doilea call `X` este deja legat și nu se poate demonstra scopul
`caesar(socrate)`.


### Unificare

Ceea ce merită clarificat este că legarea unei variabile la o valoare este un
pas necesar în procesul de unificare.

Am spus mai devreme că nicio regulă a predicatului `caesar` nu unifică cu
`caesar(socrate)`. Sau `om(X)` unifică cu `om(socrate)` legându-l pe `X` la
`socrate`.

Totuși de ce legarea este importantă în înțelgerea unificării? În exemplul de
mai jos cele două expresii diferă, deci nu ar trebui să unifice.

```prolog
?- caesar(gaiusIulius) = caesar(octavianAugustus).
false.
```

**Mai important** este că, deși diferă, nu există *nicio* legare ca să facem
expresiile să coincidă.

```prolog
?- caesar(X) = caesar(Y).
X = Y.
```

În ultimul exemplu deși cele două expresii diferă, și ele referă variabile care
nu sunt instanțiate la aceeași valoare, există cel puțin o **legare** ca să se
satisfacă scopul, și anume dacă `X` și `Y` se leagă la aceeași valoare.

```prolog
?- X = Y, string_concat("P", "P", X), writeln(Y).
PP
X = Y, Y = "PP".
```

Mai sus `X` și `Y` au unificat, iar legarea lui `X` îl leagă și pe `Y` la șirul
"PP".

### Domenii de vizibilitate

Odată legată o variabilă la o valoare, aceasta nu se mai poate modifica pentru
durata ei de viață. (Durata de viață va fi un concept clarificat pe parcursul
laboratorului.)

Atenție la următorii termeni:

- domeniu de vizibilitate a unei variabile (en. *scope*)
- [scop](https://www.swi-prolog.org/pldoc/man?section=glossary#gloss:goal) (en.
  *goal*)

```prolog
single([A]).
double([A, A]).

?- single([1]), double([2, 2]).
true.
```

În exemplul anterior am văzut că variabile denumite la fel (având același
identificator), `A` se pot lega în aceeași interogare la diferite valori, `1`
și `2`. Deși cel mai bine este să consultați standardul limbajului, de obicei
întinderea domeniului de vizibilitate a unei variabile este o singură clauză sau
o interogare.

### Negația ca eșec (\\+)

Prolog utilizează *presupunerea lumii închise*: ceea ce nu poate fi demonstrat, este fals. De aceea, în Prolog `\+ p` trebuie citit ca:

> scopul `p` nu poate fi satisfăcut
> 
> `p` nu poate fi demonstrat
>
> nu se poate găsi o legare pentru variabile din premise astfel încât predicatul
> să fie demonstrat 

Faptul că Prolog utilizează negația ca eșec (eng. *negation as failure*) are implicații asupra execuției programelor.

În logica de ordinul întâi, următoarele două expresii sunt echivalente: *¬a(X) ^
b(X)* și *b(X) ^ ¬a(X)*. În Prolog, următoarele 2 clauze (`p1` și `p2`) vor
produce rezultate diferite:

```prolog
student(andrei).
student(marius).
lazy(marius).

p1(X) :- student(X), \+ lazy(X).
p2(X) :- \+ lazy(X), student(X).

?- p1(X).
X = andrei ;
false. % se încearcă și găsirea următorului student care satisface cea de-a doua
% premisă, însă nu se poate

?- p2(X).
false.
```

Acesta se întâmplă pentru că, Prolog **nu poate să derive, pe baza negației,
legări** pentru `X`. În Prolog putem folosi negația doar pentru a *verifica*
variabile deja legate sau pentru a exprima faptul că *nu se poate demonstra că
predicatul este adevărat*.

În `p1`, `X` este legat și negația are rolul de a verifica că `lazy` nu este
adevărat pentru `X`. În `p2`, `X` este nelegat, deci putem interpreta
rezultatele folosind a doua modalitate: Prolog va încerca să demonstreze că nu
există `X` pentru care `lazy` să fie adevărat, ceea ce nu este corect.

## Câteva observații asupra purității

În logica de ordinul întâi clauzele *p(A, B) ^ q(A, B)* și *q(A, B) ^ p(A, B)*
sunt echivalente. Ordinea termenilor dintr-o conjuncție (sau disjuncție) nu
influențează valoarea de adevăr a clauzei.

În Prolog acest lucru nu este întotdeauna adevărat:

```prolog
?- X = Y, X == Y. 
X = Y.

?- X == Y, X = Y. 
false.
```

## Puterea generativă a limbajului

Așa cum am văzut în laboratorul precedent scopurile pot fi privite ca întrebări
ale căror răspunsuri sunt `true` sau `false`. În plus, acest răspuns poate fi
însoțit de legări variabilelor din cadrul scopului.

Pentru a obține lungimea unei liste putem folosi:

```prolog
% lungime(+Lista, -Lungime)
lungime([], 0).
lungime([_ | R], N) :- lungime(R, N1), N is N1 + 1.

?- lungime([1, 2, 3], N).
N = 3.
```

În exemplul de mai sus se va încerca satisfacerea scopului `lungime([1,2,3],N)`
printr-o legare convenabilă a variabilei `N`. Observați cum în interogări se
generează o legare pentru variabile în funcție de legările apelurilor recursive.

```prolog
[trace]  ?- lungime([1, 2, 3], N).
   Call: (10) lungime([1, 2, 3], _9968) ? creep
   Call: (11) lungime([2, 3], _10424) ? creep
   Call: (12) lungime([3], _10468) ? creep
   Call: (13) lungime([], _10512) ? creep
   Exit: (13) lungime([], 0) ? creep
   Call: (13) _10604 is 0+1 ? creep
   Exit: (13) 1 is 0+1 ? creep
   Exit: (12) lungime([3], 1) ? creep
   Call: (12) _10742 is 1+1 ? creep
   Exit: (12) 2 is 1+1 ? creep
   Exit: (11) lungime([2, 3], 2) ? creep
   Call: (11) _9968 is 2+1 ? creep
   Exit: (11) 3 is 2+1 ? creep
   Exit: (10) lungime([1, 2, 3], 3) ? creep
N = 3.
```

În acest caz soluția este unică, dar așa cum am văzut anterior, putem avea
situații în care există mai multe unificări posibile. Putem folosi faptul că se
încearcă resatisfacerea unui scop în mod exhaustiv pentru a genera multiple
rezultate.

```prolog
% Verifică dacă un element aparține unei liste
% membru(?Elem, +Lista)
membru(Elem, [Elem | _]).
membru(Elem, [_ | Rest]) :- membru(Elem, Rest).
```

Putem folosi acest predicat pentru a obține un răspuns:

```prolog
?- membru(3, [1, 2, 3, 4, 5]).
true.
```

Sau putem să îl folosim pentru a genera pe rând toate elementele unei liste:

```prolog
?- membru(N, [1, 2, 3]).
N = 1 ;
N = 2 ;
N = 3 ;
false.
```

Inițial, se va încerca unificarea scopul `membru(N, [1, 2, 3])` cu faptul
`membru(Elem, [Elem | _]).`. Deci ar trebui să unificăm `Elem = N` și
`Elem = 1`, ceea ce poate prin legarea `N = 1`.

Când alegem să ni se mai *genereze* un răspuns, tastând `;`, se va încerca
unificarea cu antetul de regulă `membru(Elem, [_ | Rest])`, în care `Elem = N`,
iar `Rest = [2, 3]`. Această încercare implică satisfacerea unui nou scop,
`membru(N, [2, 3])`. Noul scop va unifica, de asemenea, cu primul fapt,
`membru(Elem, [Elem | _])`, din care va rezulta `N = 2`.

Asemănător se va găsi și soluția `N = 3`, după care nu va mai reuși nicio altă
unificare.

Pentru a exemplifica utilizarea acestui mecanism, vom considera următorul
exemplu în care dorim generarea, pe rând, a tuturor permutărilor unei liste.
Definim mai întâi un predicat ajutător care șterge un element dintr-o listă.

```prolog
% remove(+Elem, +Lista, -ListaNoua)
remove(Elem, [Elem | Rest], Rest).
remove(Elem, [Head | Rest], [Head | Left]) :- remove(Elem, Rest, Left).
```

Ca să generăm o permutare pentru o listă `[Head | Rest]`, vom genera mai întâi o
permutare pentru lista `Rest`. Apoi ne vom folosi de predicatul `remove` pentru
a insera pe `Head` pe diferite poziții în această "subpermutare".

```prolog
% perm(+Lista, -Permutare)
perm([], []).
perm([Head | Rest], Perm) :- perm(Rest, P1), remove(Head, Perm, P1).
```

Observați că în a doua premisă, `remove(Head, Perm, P1)`, `P1` este deja legat
dacă s-a reușit satisfacerea primei premise, `perm(Rest, P1)`. Cazul de bază
`perm([], [])` ne asigură această reușită mereu.

Folosim predicatul `remove` cu al doilea parametru nelegat pentru a genera o
listă `Perm`. Dacă din `Perm` l-am șterge pe `Head` ar rezulta `P1`. Urmăriți
exemplul de mai jos.

```prolog
?- remove(3, L, [1, 1, 1]).
L = [3, 1, 1, 1] ;
L = [1, 3, 1, 1] ;
L = [1, 1, 3, 1] ;
L = [1, 1, 1, 3] ;
false.
```

Iar acesta este comportamentul predicatului final:

```prolog
?- perm([1, 2, 3], Perm).
Perm = [1, 2, 3] ;
Perm = [2, 1, 3] ;
Perm = [2, 3, 1] ;
Perm = [1, 3, 2] ;
Perm = [3, 1, 2] ;
Perm = [3, 2, 1] ;
false.
```

Putem folosi puterea generativă a limbajului pentru a produce soluții bazate pe combinații. De exemplu, dacă ne dorim toate perechile de numere (ca soluții succesive) din listele `L1` și `L2`, dar a căror sumă *nu* este în lista `L3`, putem folosi interogarea:

`?- [L1,L2,L3]=[[1,2,3], [4,5,6], [5,6]], member(X, L1), member(Y, L2), S is X + Y, \+ member(S, L3).`

## Controlul execuției: operatorul cut (`!`) și `false`

### Predicatul false

`false` (echivalent cu `fail`, dar adăugat mai de curând pentru o interpretare mai clară) este un predicat care eșuează întotdeauna. De ce am vrea să scriem o regulă care să eșueze? Un posibil răspuns este: datorită efectelor pe care le pot avea scopurile satisfăcute până la întâlnirea predicatului `false`. Un exemplu este următorul:

```prolog
my_reverse(List, Acc, _) :- format('List:~w, Acc:~w~n', [List, Acc]), false.
my_reverse([], Sol, Sol).
my_reverse([Head | Tail], Acc, Sol):-my_reverse(Tail, [Head | Acc], Sol).
```

În codul de mai sus, am scris o regulă pentru funcția `my_reverse` care are un singur rol: acela de a afișa argumentele `List` și `Acc` în orice moment se dorește satisfacerea unui scop cu predicatul `my_reverse/3`. Regula însă nu va avea efect asupra restului execuției, din moment ce ea eșuează întotdeauna.

```prolog
?- my_reverse([1,2,3,4],[],Rev).
List:[1,2,3,4], Acc:[]
List:[2,3,4], Acc:[1]
List:[3,4], Acc:[2,1]
List:[4], Acc:[3,2,1]
List:[], Acc:[4,3,2,1]
Rev = [4, 3, 2, 1].
```

### Predicatul cut

În Prolog, predicatul cut (`!`) are rolul de a elimina toate punctele de bifurcație create în predicatul curent. La evaluarea predicatului cut într-un predicat `p`, se vor realiza două tipuri de efecte:

  * nu se vor mai genera soluții (dacă este nevoie, sau dacă soluția curentă eșuează) pentru alte reguli ale predicatului `p`
  * nu se vor mai genera soluții (dacă este nevoie, sau dacă soluția curentă eșuează), pentru alte soluții ale condițiilor care apar **în aceeași regulă cu cut**, și înainte de cut.


De exemplu, în programul:
```prolog
p(a).
p(b).
p(A/B) :- q(A), !, t(A/B).
p(d).

q(a).
q(b).
q(c).

t(a/a).
t(a/b).
t(b/c).
t(b/d).
```

Interogarea `?- p(X).` va întoarce soluțiile:
```prolog
X = a ;
X = b ;
X = a/a ;
X = a/b.
```

Primele două soluții sunt soluții date de primele două reguli pentru `p`. Pentru următoarea soluție, Prolog încearcă regula cu cut. Pentru `q(A)`, sunt trei alternative: `A=a, A=b, A=c`. La încercarea primeia, însă, urmează predicatul cut, care anulează:

  * alternativele pentru `q`, deci nu se vor mai considera posibilitățile `A=b` și `A=c`
  * alternativele pentru `p`, deci nu se va mai considera regula `p(d)`

Alternativele pentru `t` sunt însă considerate normal, pentru că acestea se creează *după* evaluarea lui cut.

Putem utiliza predicatul cut în două moduri:

  * atunci când știm că am ajuns la soluția care ne interesează, și știm că nu mai avem nevoie de o altă soluție pentru predicat,  putem utiliza cut pentru a nu mai explora alte soluții (cut verde / *green cut*).
  * atunci când dorim în mod explicit ca Prolog să nu mai exploreze alte posibilități pentru același predicat, pentru că acestea nu ar genera soluții corecte, dacă se aplică regula curentă (cut roșu / *red cut*).

**Exemplu:** implementarea predicatului `min`.

**Varianta 1** -- fără cut:
```prolog
min(X, Y, Min) :- X < Y, X = Min. % regula 1
min(X, Y, Min) :- X >= Y, Y = Min. % regula 2
```

Este important să avem comparația din regula 2, în lipsa ei obținând, pentru cazul în care `X < Y`, o a doua soluție falsă, în care Y este minimul. Testați cu:

```prolog
minB(X, Y, Min) :- X < Y, X = Min. % regula 1
minB(_, Y, Min) :- Y = Min. % regula 2
```

Pentru interogarea `minB(2, 3, Min)` se obțin două soluții: `Min=2` și `Min=3`.

**Varianta 2** Putem integra predicatul cut ca un cut verde astfel:

```prolog
min2(X, Y, Min) :- X < Y, !, X = Min. % regula 1
min2(X, Y, Min) :- X >= Y, Y = Min. % regula 2
```

Este o utilizare de tip "green cut" - cut poate să lipsească și execuția nu devine incorectă, dar este îmbunătățită prin faptul că atunci când `X < Y` Prolog va ști să nu mai intre (nu mai este nevoie să intre) pe a doua regulă.

Observați că este important ca predicatul cut să fie pus atunci când știm cu siguranță ca suntem pe ramura corectă. Dacă, de exemplu, implementăm astfel:

```prolog
min2B(X, Y, Min) :- !, X < Y, X = Min.
min2B(X, Y, Min) :- X >= Y, Y = Min.
```
Pentru `min2B(3, 2 ,M)`, se va evalua predicatul cut (care anulează alternativa pentru min2B), inegalitatea eșuează, și interogarea va eșua, pentru că din cauza lui cut Prolog nu mai intră și pe a doua regulă.

**Varianta 3** Putem integra predicatul cut ca un cut roșu astfel:

```prolog
min3(X, Y, Min) :- X < Y, !, X = Min.
min3(_, Y, Min) :- Y = Min.
```

Dacă inegalitatea din prima regulă reușește, sigur nu mai avem nevoie de a doua regulă.

Aici, cut **nu** poate să lipsească -- dacă lipsește vom obține și soluții incorecte, ca în cazul lui `minB`.

## Obținerea de soluții prin generare și testare

### Considerente teoretice

Un algoritm este nedeterminist dacă poate alege următoarea sa stare. Deci are
mai multe căi de execuție, fiecare cu rezultatul ei.

Prolog este un limbaj care implementează un model nedeterminist de execuție. În general, algoritmii nedeterminiști au două etape:

1. Generarea tuturor valorilor care respectă o anumită structură.
2. Verificarea dacă o valoare este sau nu o soluție.

### Exemplu

Fie problema identificării unei submulțimi de sumă dată. (en. *subset sum*) O
rezolvăm cât mai simplu, mai întâi:

```prolog
% subset_sum(+List, ?Sum).
subset_sum(_, 0).
subset_sum([_ | Rest], Sum) :- subset_sum(Rest, Sum).
subset_sum([Head | Rest], Sum) :- subset_sum(Rest, S1), Sum is S1 + Head.

?- subset_sum([4, 2, 1], 0).
true .
?- subset_sum([4, 2, 1], 7).
true.
?- subset_sum([4, 2, 1], 8).
false.
```

Observați că premisa `subset_sum(Rest, S1)` generează legări pentru `S1` care
s-ar putea dovedi nefolositoare în premisa de testare, `Sum is S1 + Head`.

## Backtracking atunci când cunoaștem dimensiunea soluției

```prolog
% subset_sum(+List, +Sum).
subset_sum(_, 0) :- !.
subset_sum([Head | Rest], Sum) :- Head > Sum, !, subset_sum(Rest, Sum).
subset_sum([Head | Rest], Sum) :-
    S1 is Sum - Head,
    subset_sum(Rest, S1).
```

Considerăm adăugarea primului element **doar** dacă este mai mic decât suma
cerută. Apoi am renunțat la posibilitatea folosirii variabilei `Sum`
neinstanțiate pentru a eficientiza apelurile recursive.

## Aflarea tuturor soluțiilor pentru satisfacerea unui scop

Prolog oferă un set special de predicate care pot construi liste din toate soluțiile de satisfacere a unui scop. Acestea sunt extrem de utile deoarece altfel este complicată culegerea informațiilor la revenirea din backtracking (o alternativă este prezentată în secțiunea următoare).

**Observație**: Orice legare din scopurile pasate ca parametru funcțiilor de mai
jos **NU** nu se menține după încercarea de satisfacere.

### findall(+Template, +Goal, -Bag)

Predicatul `findall` pune în `Bag` câte un element Template pentru fiecare soluție a expresiei `Goal`. Desigur, predicatul este util atunci când `Goal` și `Template` au variabile comune. De exemplu: 
```prolog
even(Numbers, Even):-
    findall(X,
            (member(X, Numbers), X mod 2 =:= 0),
            Even).

?- even([1, 2, 3, 4, 5, 6, 7, 8, 9], Even).
Even = [2, 4, 6, 8].
```

### forall(+Cond, +Action)
Predicatul `forall/2` verifică dacă pentru orice legare din `Cond`, care reprezintă un domeniu ce conține legări de variabile, se pot îndeplini condițiile din `Action`.

Exemple:
```prolog
?- forall(member(X, [2, 4, 6]), X mod 2 =:= 0).
true.

?- forall(member(X, [2, 4, 3, 6]), X mod 2 =:= 0).
false.

?- forall(member(X, [6, 12, 18]), (X mod 2 =:= 0, X mod 3 =:= 0)).
true.
```

## Resurse
-   [Cheatsheet](https://github.com/cs-pub-ro/PP-laboratoare/raw/master/prolog/legare-executie/prolog_cheatsheet-2.pdf)
-   [Schelet](https://ocw.cs.pub.ro/courses/_media/pp/23/laboratoare/prolog/legare-executie-schelet.zip)
-   [Soluții](https://ocw.cs.pub.ro/courses/_media/pp/23/laboratoare/prolog/legare-executie-solutii.zip)

