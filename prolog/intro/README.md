# Prolog: Introducere
  - Data publicării: 8.05.2022
  - Data ultimei modificări: 8.05.2022

## Obiective

Scopul acestui laborator este introducerea în programarea logică și învățarea primelor noțiuni despre Prolog.

Aspectele urmărite sunt:

  - diferențierea dintre aceasta paradigmă și cele studiate anterior
  - familiarizarea cu entitățile limbajului: **fapte**, **reguli**, **scopuri**
  - sintaxa **Prolog**
  - structuri de date


## SWI-Prolog

Folosim SWI-Prolog, așa cum este detaliat
[aici](..:..:limbaje#prolog "wikilink").

În cadrul laboratorului, recomandăm folosirea **swipl** în modul următor:

* se rulează comanda comanda `swipl` în terminal, sau direct `swipl fisier.pl` pentru a încărca un fișier existent
    * alternativ, se poate folosi comanda prolog
* se salvează faptele și regulile într-un fișier cu extensia `.pl`
* pentru a invoca editorul, se pot folosi următoarele comenzi:
    * `edit.`, dacă a fost deja încărcat un fișier
    * `edit(file('new.pl')).`, pentru a crea un fișier nou
    * `edit('new.pl')`, dacă fișierul a fost creat anterior
* dacă fișierul nu a fost încărcat prin argument în linia de comandă, se încarcă folosind comanda `consult('file.pl').`
* pentru a reîncărca toate fișierele modificate de la ultima încărcare, se folosește comanda `make.`
* pentru a ieși din terminalul `swipl`, se folosește comanda `halt.`


## Comentarii

Simbolul `%` transformă restul rândului într-un comentariu.

## Entitățile limbajului

Limbajul Prolog (al cărui nume provine de la Programmable Logic) este un limbaj **logic**, **descriptiv**, care permite specificarea problemei de rezolvat în termenii unor **fapte** cunoscute despre obiectele universului problemei și ai relațiilor existente între aceste obiecte.

Tot ceea ce nu este cunoscut sau nu poate fi demonstrat este considerat a fi fals (**ipoteza lumii închise**).

Execuția unui program Prolog constă în deducerea implicațiilor acestor **fapte** și **relații**, programul definind astfel o mulțime de consecințe ce reprezintă înțelesul sau semnificația declarativă a programului.

Un program Prolog conține următoarele **entități**:

  * **fapte** despre obiecte și relațiile existente între aceste obiecte
  * **reguli** despre obiecte și relațiile dintre ele, care permit deducerea (inferarea) de noi fapte pe baza celor cunoscute
  * întrebări, numite și **scopuri**, despre obiecte și relațiile dintre ele, la care programul răspunde pe baza faptelor și regulilor existente

## Fapte

Faptele sunt **predicate de ordinul întâi** de aritate *n*, considerate **adevărate**. Ele stabilesc relații între obiectele universului problemei. Numărul de argumente ale faptelor este dat de **aritatea** (numărul de argumente) corespunzătoare a predicatelor.

Exemple de fapte: 
```prolog
papagal(coco).
iubeste(mihai, maria).
iubeste(mihai, ana).
frumoasa(ana).
bun(gelu).
deplaseaza(cub, camera1, camera2).
```

## Structuri

Structurile au aceeași sintaxă cu faptele, dar apar ca argumente ale predicatelor.

Exemplu de structură: 
```prolog 
are(ion,carte(aventuri,2002)).
```

## Scopuri

Obținerea consecințelor sau a rezultatului unui program Prolog se face prin fixarea unor **scopuri** care pot fi **adevărate** sau **false**, în funcție de conținutul **bazei de cunoștințe** Prolog. Scopurile sunt predicate pentru care se dorește aflarea valorii de adevăr în contextul faptelor existente în baza de cunoștințe.

Cum scopurile pot fi văzute ca **întrebări**, rezultatul unui program Prolog este răspunsul la o întrebare (sau la o conjuncție de întrebări). Acest răspuns poate fi afirmativ, **true**, sau negativ, **false** (în alte versiuni de Prolog răspunsul poate fi **yes** sau **no**; sau **true** sau **fail**).

Se va vedea mai târziu că programul Prolog, în cazul unui răspuns afirmativ la o întrebare, **poate furniza și alte informații** din baza de cunoștințe.

Considerând baza de cunoștințe specificată anterior, se pot pune diverse întrebări, cum ar fi: 
```prolog
?- iubeste(mihai, maria).
true.
?-papagal(coco).
true.
?- papagal(mihai).
false.
?- inalt(gelu).
false.
```

## Variabile

În exemplele prezentate până acum, argumentele **faptelor** și **întrebărilor** au fost obiecte particulare, numite și **constante** sau **atomi simbolici**. Predicatele Prolog, ca orice predicate în logica cu predicate de ordinul I, admit ca argumente și obiecte generice numite **variabile**.

În Prolog, prin convenție, numele argumentelor variabile începe cu **literă mare** iar numele constantelor simbolice începe cu **literă mică**.

O variabilă poate fi **instanțiată** (legată) dacă există un obiect asociat acestei variabile, sau **neinstanțiată** (liberă) dacă nu se știe încă ce obiect va desemna variabila.

Semnul *\_* (underscore) desemnează o variabila a cărei valoare nu interesează.

```prolog 
?- papagal(coco).
true.
?- papagal(CineEste).
CineEste = coco
?- deplaseaza(_, DeUnde, Unde).
DeUnde = camera1, Unde = camera2

```

La fixarea unui **scop** Prolog care conține **variabile**, acestea sunt neinstanțiate iar sistemul încearcă satisfacerea acestui scop căutând printre faptele din baza de cunoștințe un fapt care poate identifica cu scopul, printr-o **instanțiere adecvată a variabilelor** din scopul dat. Este vorba de fapt de un proces de **unificare** a predicatului scop cu unul din predicatele fapte existente în baza de cunoștințe.

În exemplul de mai jos exista mai multe răspunsuri posibile. Prima soluție este dată de prima unificare și există atâtea soluții câte unificări diferite există.

```prolog 
?- iubeste(mihai, X).
```

La realizarea primei unificări se **marchează** faptul care a unificat și care reprezintă prima soluție. La obținerea următoarei soluții, căutarea este reluată de la marcaj în jos în baza de cunoștințe.

Obținerea primei soluții este de obicei numită **satisfacerea scopului** iar obținerea altor soluții, **resatisfacerea scopului**.

La satisfacerea unui scop căutarea se face întotdeauna de la începutul bazei de cunoștințe. La resatisfacerea unui scop, căutarea se face începând de la marcajul stabilit de satisfacerea anterioară a acelui scop.

Sistemul Prolog, fiind un sistem **interactiv**, permite utilizatorului obținerea fie a primului răspuns, fie a tuturor răspunsurilor. În cazul în care, după afișarea tuturor răspunsurilor, un scop nu mai poate fi resatisfăcut, sistemul răspunde **false**.

In exemplul de mai jos, tastând caracterul “;” și Enter, cerem o nouă soluție.

```prolog 
?- iubeste(mihai, X). 
X = maria;
X = ana;
false.
?- iubeste(Cine, PeCine).
Cine = mihai, PeCine = maria;
Cine = mihai, PeCine = ana;
false.
```

## Reguli

O regulă Prolog exprimă un fapt care depinde de alte fapte și este de forma:

```prolog
S :- S1, S2, ..., Sn.
```

Fiecare *S_i*, *i = 1,n* și *S* au forma faptelor Prolog, deci sunt predicate, cu argumente constante, variabile sau structuri. Faptul S care definește regula, se numește **antet de regulă**, iar *S_1, S_2,..., S_n* formează corpul regulii și reprezintă conjuncția de scopuri care trebuie satisfăcute pentru ca antetul regulii să fie satisfăcut.

Fie următoarea bază de cunoștințe:

 ```prolog
frumoasa(ana).                                         %1
bun(vlad).                                             %2
cunoaste(vlad, maria).                                 %3
cunoaste(vlad, ana).                                   %4
iubeste(mihai, maria).                                 %5
iubeste(X, Y):- bun(X), cunoaste(X, Y), frumoasa(Y).   %6

```

Se observă definirea atât printr-un fapt (linia 5), cât și printr-o regulă (linia 6) a predicatului *iubeste(?Cine, ?PeCine)*.


## Operatori

  - Aritmetici: `+` `-` `*` `/`
  - Relaționali: `=\=` `<` `>` `=<` `>=` `=:=` `is`
  - Logici: `,` (și) `;` (sau) `\+` (negație)

La scrierea expresiei `1+2\*(X/Y)`, valoarea acesteia nu este calculată, ci expresia este reținută ca atare. Se poate observa că operatorii `=:=` și `is` forțează evaluarea unei expresii, pe când `=` verifica doar egalitatea structurală.

De asemenea, `is` și `=` pot primi variabile neinstanțiate pe care le instanțiază (`is` doar în partea stângă).

```prolog

?- 1 + 2 =:= 2 + 1.
true.

?- 1 + 2 = 2 + 1. 
false.

?- X = 2 + 1. 
X = 2+1.

?- X is 2 + 1. 
X = 3.

?- X =:= 2 + 1. 
ERROR: =:=/2: Arguments are not sufficiently instantiated
```
### Negație

Operatorul unar `\+` folosit pentru un operand reprezintă faptul că nu se poate demonstra că operandul este adevărat.
Dacă operandul conține variabile, `\+` denotă că nu există nicio legare pentru variabile astfel încât operandul să fie adevărat.
Operatorul `\+` trebuie în mod necesar să fie urmat de spațiu (sau paranteză deschisă)

## Liste

  - Lista vidă: `[]`
  - Lista cu elementele a, b, c: `[a,b,c]`
  - Lista nevidă: `[Prim|Rest]` – unde variabila `Prim` unifică cu primul element al listei, iar variabila `Rest` cu lista fără acest prim element
  - Lista care începe cu n elemente `X1, X2, ..., XN` și continuă cu o altă listă `Rest`: `[X1,X2,...,XN|Rest]`

## Documentarea predicatelor și a argumentelor

Pentru claritate, antetele predicatelor se scriu sub forma:

  * **predicat/nrArgumente**
  * predicat(+Arg1, -Arg2, ?Arg3, ..., +ArgN)

Pentru a diferenția intrările (+) de ieșiri (-), se prefixează argumentele cu indicatori. Acele argumente care pot fi fie intrări, fie ieșiri se prefixează cu '?'. Instanțierea parametrilor ține de specificarea acestora:

* Arg1 va fi deja instanțiat atunci când se va încerca satisfacerea p/3
* Arg2 va fi neinstanțiat atunci când se va încerca satisfacerea p/3 , și dacă predicatul este satisfăcut, Arg2 va fa fi instanțiat la finalul evaluării lui p/3; Arg2 poate fi și deja instanțiat la evaluarea lui p/3, și atunci evaluarea lui p/3 poate servi ca verificare a corectitudinii argumentului în raport cu semnificația prediatului
* Arg3 va putea fi instanțiat sau nu atunci când se va încerca satisfacerea p/3



## Resurse
-   [Cheatsheet](https://github.com/cs-pub-ro/PP-laboratoare/raw/master/prolog/intro/prolog-cheatsheet-1.pdf)
-   [Schelet](https://ocw.cs.pub.ro/courses/_media/pp/22/laboratoare/prolog/intro-schelet.zip)
-   [Soluții](https://ocw.cs.pub.ro/courses/_media/pp/22/laboratoare/prolog/intro-solutie.zip)

## Referințe

  - [Learn prolog now\!](http://www.learnprolognow.org/ "wikilink")
  - [Logic, Programming, and Prolog](http://www.ida.liu.se/~ulfni53/lpp/bok/bok.pdf "wikilink")
  - [Built-in Predicates](http://www.swi-prolog.org/pldoc/doc_for?object=section%281,%274%27,swi%28%27/doc/Manual/builtin.html%27%29%29 "wikilink")

