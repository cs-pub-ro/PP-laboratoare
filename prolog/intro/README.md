# Prolog: Introducere
  - Data publicării: 1.05.2023
  - Data ultimei modificări: 1.05.2023
  
## Obiective

Scopul acestui laborator este introducerea în programarea logică și învățarea
primelor noțiuni despre Prolog.

Aspectele urmărite sunt:

  - diferențierea dintre aceasta paradigmă și cele studiate anterior
  - familiarizarea cu entitățile limbajului: **fapte**, **reguli**, **scopuri**
  - sintaxa **Prolog**
  - structuri de date

## Motivație

### Prolog

[Prolog](https://en.wikipedia.org/wiki/Prolog) a fost unul dintre
[primele](https://en.wikipedia.org/wiki/Logic_programming#History) limbaje de
programare **logice** și rămâne în continuare cel mai popular astfel de limbaj,
folosit în demonstratoarele de teoreme și utilizat inclusiv pentru o parte din
implementarea sistemului IBM Watson. Permite separarea datelor de procesul de
inferență, programele fiind scrise într-un stil **declarativ** și uniform.
Impactul principal l-a avut în cercetare, în domeniul inteligenței artificiale,
dar a rămas un punct de inspirație pentru limbajele de după.

Numele limbajului este o abreviere pentru *programmation en logique*, și este
bazat pe calcul cu predicate.

### Recapitulare teorie

Logica cu predicate de ordin I este o extensie a logicii propoziționale, prin
folosirea de variabile cuantificate pentru a stabili relații. Urmăriți
următoarele două exemple de transformare din logica propozițională în cea cu
predicate de ordin I.

> Toții peștii respiră. (prin branhii)

*∀ X . (peste(X) → respira(X))*

> Unii pești au o respirație aeriană. (prin plămâni)

*∃ X . (peste(X) ∧ respira(X))*

Limbajul restricționează această logică doar la folosirea de clauze Horn. O
clauză este o disjuncție (o operație *sau*) peste predicate sau și negații de
predicate. O **clauză Horn** conține un singur literal pozitiv, ceea ce înseamnă
că este o implicație care nu poate avea drept concluzie o disjuncție între mai
multe predicate. (Vezi cursul pentru o înțelegere mai bună.)

A_1 ∧ A_2 ∧ ... ∧ A_n → A

true → B

Deci următoarea implicație **nu** poate fi transcrisă direct într-o regulă în
Prolog, pentru că are ca implicație o disjuncție dintre două predicate.

int(a) ∧ int(b) ∧ a ≠ 0 ∧ sum(a, b) = 0 → negative(a) ∨ negative(b)

### SWI-Prolog

SWI-Prolog este o
[implementare](https://en.wikipedia.org/wiki/Prolog#ISO_Prolog) open-source a
limbajului, dispunând de multe biblioteci, fiind un punct bun de plecare pentru
tranziția către alte limbaje logice/implementări.

Dintre [toate
implementările](https://en.wikipedia.org/wiki/Comparison_of_Prolog_implementations)
fiecare are particularitățile ei sintatice sau de folosire. De aceea vă rugăm să
urmăriți instrucțiunile de [aici](..:..:limbaje#prolog "wikilink") pentru
laborator.

## Sintaxă și semantică

Programele scrise în Prolog descriu relații exprimate prin clauze. Există două
tipuri de clauze:

- axiome sau fapte (en. *facts*)
- reguli

"Calculul" modelează în această paradigmă efectuarea de raționamente.

### Axiome (Fapte)

Axiomele, sau faptele, sunt predicate de ordinul I de
[aritate](https://ro.wikipedia.org/wiki/Aritate) *n*, considerate **adevărate**.
Ele stabilesc relații între obiectele universului problemei. Exemple:

```prolog
% Acesta este un comentariu.
% Predicate de aritate 1. (unare)
caine(cerberus).
om(socrate). 
muritor(leulDinNemeea).
muritor(rhesus).

% Predicate de aritate 2. (binare)
% cel_mai_bun_prieten(?Cine, ?AlCui)
cel_mai_bun_prieten(cerberus, hades).

% Predicate de aritate 3. (ternare)
% rege(?Nume, ?Regiune, ?Aliat)
rege(rhesus, tracia, troia).
```

Rulăm următoarele interogări:

```prolog
?- caine(cerberus). % este cerberus un câine?
true.

?- muritor(socrate). % vom detalia mai jos
false.
```

### Termeni

În Prolog orice valoare se numește
[termen](https://www.swi-prolog.org/pldoc/man?section=glossary#gloss:term).
Tipuri simpli de termeni:

- Constante textuale, sau mai bine zis *atomi*
  ```prolog
  nume, ion, popescu
  ```
- Numere întregi/numere în virgulă mobilă
- Variabile (vom detalia mai departe)
  ```prolog
  ?- om(X).
  X = socrate.
  ```
- Termeni
  [compuși](https://www.swi-prolog.org/pldoc/man?section=glossary#gloss:compound).
  ```prolog
  % exemplu structură
  client(nume(ion, popescu), carte(aventuri, 2002)).
  ```

Cuvântul
[structură](https://www.swi-prolog.org/pldoc/man?section=glossary#gloss:structure)
este un sinonim pentru termenul compus.

Puteți considera momentan că singura diferență, sintactică, este că predicatele
nu sunt transmise ca argumente, aceasta fiind o
[discuție](https://stackoverflow.com/questions/28972038/prolog-structurecomplex-term-vs-predicate-i-dont-really-get-the-difference)
mai subtilă ce ține de reprezentarea internă a implementării.

Consultați
[**glosarul**](https://www.swi-prolog.org/pldoc/man?section=glossary) pentru
orice detalii suplimentare.

### Scopuri și variabile

Când rulăm interogări despre termeni și relațiile dintre ei spunem informal că
demonstrăm sau obținem informații pornind de la "baza noastră de date" (de la
axiome, de la fapte).

Calcul se face prin încercarea de a satisface [^1] *scopuri* (en. *goals*).

**OBSERVAȚIE**: Când am interogat dacă Socrate este muritor, procesul de
execuție a returnat `false` deoarece **nu** se putea satisface acest scop. Nu
înseamnă că el este nemuritor. Aceasta este *ipoteza lumii închise* -- orice nu
poate fi demonstrat ca adevărat va fi considerat fals.

```prolog
% Este Rhesus muritor și rege al Traciei, aliat al Troiei?
% Avem două scopuri de satisfăcut
?- muritor(rhesus), rege(rhesus, tracia, troia).
true.

% Cine este muritor?
?- muritor(X).       % tastăm o interogare cu un singur scop
X = leulDinNemeea ;  % tastăm ";" pentru a primi încă un răspuns
X = rhesus.
```

Observați că în a doua interogare am făcut primul nostru calcul util, folosind o
variabilă, `X`. Argumentul nu mai este o valoare particulară, ci sistemul de
execuție încearcă **legarea** ei la diferite constante sau atomi. Prin convenție
numele variabilelor (`X`) începe cu literă mare iar numele atomilor
(`leulDinNemeea`, `rhesus`) începe cu literă mică.

Așa cum v-ați obișnuit de la Haskell, și Prolog permite folosirea de variabile
[anonime](https://www.swi-prolog.org/pldoc/man?section=glossary#gloss:anonymous),
`_`. Multiple folosiri ale lui `_` nu se leagă la același termen.

```prolog
?- muritor(X), rege(X, Y, _).
X = rhesus,
Y = tracia.

?- muritor(X), rege(X, _, _).
X = rhesus.
```

### Reguli

```
Antet :- Corp.
```

O regulă este o declarație cu forma generală de mai sus, unde antentul este un
predicat, iar corpul este alcătuit din premise separate de operatori. Ea se
citește așa:

> Antetul este adevărat dacă corpul este adevărat

Practic este o implicație de la dreapta la stânga.

Exemple:

```prolog
om(socrate) :- true.
% Echivalent cu: om(socrate).

viu(hercule).   % semi-zeu, nici om, nici zeu
viu(zeus).      % regele zeilor de pe Muntele Olimp
viu(hunabku).   % tatăl zeilor în mitologia mayașă 

zeu(zeus).      % fapt adevărat în mitologia greacă

muritor(X) :- om(X).
muritor(X) :- viu(X), \+ zeu(X).
```

Observați că:

- Axiomele sunt reguli fără corp.
- Am folosit operatorul `,`, "și" logic ($\land$), deci a doua regulă a
  predicatului `muritor(?Cine)` are două premise.
- Am folosit
  operatorul `\+` ([doc](https://www.swi-prolog.org/pldoc/doc_for?object=(%5C%2B)/1))
  pe care îl puteți trata ca pe o negație pentru moment.

Rulăm următoarele interogări:

```prolog
?- muritor(socrate).
true .

?- muritor(zeus).
false.
```

#### Procesul de execuție

Ca să începem să înțelegem cum se execută o interogare activăm modul trace.

```prolog
?- trace.
true.

[trace]  ?- muritor(hercule).
   Call: (10) muritor(hercule) ? creep
   Call: (11) om(hercule) ? creep
   Fail: (11) om(hercule) ? creep
   Redo: (10) muritor(hercule) ? creep
   Call: (11) viu(hercule) ? creep
   Exit: (11) viu(hercule) ? creep
   Call: (11) zeu(hercule) ? creep
   Fail: (11) zeu(hercule) ? creep
   Redo: (10) muritor(hercule) ? creep
   Exit: (10) muritor(hercule) ? creep
true.
```

Observăm că mai întâi încearcă demonstrarea primei reguli pentru predicatul
`muritor` și eșuează. Prima premisă din a doua regulă este adevărată (`viu(X)`).
Observăm că **eșecul demonstrației** scopului `zeu(hercule)` determină adevărată
a doua premisă (`\+ zeu(X)`). Cele două premise fiind puse în conjuncție,
considerăm că Hercule este muritor.

#### Negația ca eșec în demonstrație

```prolog
[trace]  ?- muritor(hunabku).
   Call: (10) muritor(hunabku) ? creep
   Call: (11) om(hunabku) ? creep
   Fail: (11) om(hunabku) ? creep
   Redo: (10) muritor(hunabku) ? creep
   Call: (11) viu(hunabku) ? creep
   Exit: (11) viu(hunabku) ? creep
   Call: (11) zeu(hunabku) ? creep
   Fail: (11) zeu(hunabku) ? creep
   Redo: (10) muritor(hunabku) ? creep
   Exit: (10) muritor(hunabku) ? creep
true.
```

În cazul lui Hunabku satisfacerea primei premise celei de-a doua reguli cât și
eșecul demonstrației că este zeu, ne determină să îl considerăm muritor.
*Totuși* deși grecii nu îl considerau zeu, el nu este un muritor, deci de unde
contradicția?!

Folosirea operatorului `\+` nu ne-a ajutat, întrucât el **întoarce adevărat dacă
nu se poate satisface argumentul**, nu este echivalent cu operatorul boolean de
negație.

**Observație**: Operatorul de negație `not` este *deprecated*.

#### Procesul de execuție 2

De asemenea, nu putem să "corectăm" greșeala anterioară prin "hardcodarea"
valorii `false`, ca mai jos, întrucât procesul de execuție încearcă în ordinea
din fișier toate declarațiile unui predicat până la satisfacerea acelui scop.

```prolog
muritor(hunabku) :- false. % declarație ineficace
```

Deci regulile pentru `muritor` pot fi "condensate" într-una singură, prin
folosirea operatorului sau `;`.

```prolog
muritor(X) :- om(X); viu(X), \+ zeu(X).
```

Aici apare prima distincție între logica formală și cea computațională:
premisele din corpul unui reguli sunt parcurse de la stânga la dreapta când se
satisface un scop. Deci dacă `om(X)` este adevărat, nu se mai caută satisfacerea
a ce a rămas din corp. Presupunem că `om(X)` se evaluează la `false`, dacă
`viu(X)` nu se poate satisface, nici nu se mai verifică `\+ zeu(X)`

V-ați obișnuit deja cu noțiunile exemplificate mai sus când verificați în `C`
dacă un pointer la o structură nu este null înainte să îl dereferențiați.

```c
// verificarea celei de-a doua condiții se efectuază doar dacă se trece de prima
if (ptr != NULL && ptr->field != ILLEGAL_VALUE) {
  // do something usefull
}
```

### Operatori

  - Aritmetici: `+` `-` `*` `/`
  - De unificare: `=` `\=` `==` `\==`
  - Relaționali aritmetici: `=\=` `<` `>` `=<` `>=` `=:=` `is`
  - De control: `,` (și) `;` (sau) `\+` (negație)
  
#### Unificare
    
Înainte să explicăm *"egalitatea"*, este dezirabilă o discuție despre
[unificare](https://www.swi-prolog.org/pldoc/man?section=glossary#gloss:unify),
adică despre operatorul `=`. 

> Unificarea = procesul de identificare a valorilor variabilelor din 2 sau mai
> multe expresii, astfel încât substituirea variabilelor prin valorile asociate
> sa conducă la coincidența expresiilor

```prolog
?- foo(a, B) = foo(A, b).
A = a,
B = b.
```

#### Diferitele tipuri de *"egalitate"*

- `=` ([doc](https://www.swi-prolog.org/pldoc/doc_for?object=(%3D)/2)): operatorul de
  unificare. Dacă operanzii nu conțin variabile, atunci verifică identitatea
  operanzilor; altfel, caută o *legare* a variabilelor în așa fel încât
  operanzii să unifice.

- `\=` ([doc](https://www.swi-prolog.org/pldoc/doc_for?object=(%5C%3D)/2)): este
  adevărat doar dacă cei doi operanzi nu pot unifica -- nu se poate găsi o
  legare a variabilelor în așa fel încât operanzii să unifice.

- `==` ([doc](https://www.swi-prolog.org/pldoc/doc_for?object=(%3D%3D)/2)) : verifică
  dacă doi operanzi sunt același lucru, iar eventualele variabile nelegate din
  operanzi sunt forțate să unifice la același lucru (printr-o unificare
  anterioară, de exemplu cu `=`).
  ```prolog
  ?- A=B, X=A, Y=B, X==Y, writeln("Adevarat").
  Adevarat
  A = B, B = X, X = Y.
  ```

- `\==` ([doc](https://www.swi-prolog.org/pldoc/doc_for?object=(%5C%3D%3D)/2)):
  echivalent cu `\+ T1 == T2`

- `is` ([doc](https://www.swi-prolog.org/pldoc/doc_for?object=(is)/2)): evaluează
  operandul din **dreapta** și
  - dacă în stânga este o variabilă nelegată, **leagă** această variabilă la
    valoarea din dreapta.
  - dacă în stânga este un număr, este echivalent cu `=:=`

- `=:=` ([doc](https://www.swi-prolog.org/pldoc/doc_for?object=(%3D%3A%3D)/2)):
  operator aritmetic care returnează adevărat dacă cele două *expresii* se
  evaluează la același *număr*. Operanzii trebuie să fie complet instanțiați.

- `=\=` ([doc](https://www.swi-prolog.org/pldoc/doc_for?object=(%3D%5C%3D)/2)):
  operator aritmetic care returnează adevărat dacă cele două *expresii* **nu**
  se evaluează la același *număr*. Operanzii trebuie să fie complet instanțiați.

Pentru o mai bună înțelegere, vom trata operatorii `=`, `is`, `==`, `\==`, `=:=`
și `=\=` din mai multe perspective.

În primul rând, din punct de vedere al necesității instanțierii variabilelor:

- pot primi variabile [neinstanțiate](https://www.swi-prolog.org/pldoc/man?section=glossary#gloss:instantiation) (adică nelegate) pe care le instanțiază: 
    - operatorul `=` poate lega în ambele părți
    ```prolog
    ?- X = 2 + 1. 
    X = 2+1.

    ?- 2 + 1 = Y.
    Y = 2+1
    
    ?- X+2 = 1+Y.
    X = 1,
    Y = 2. % atenție aici nu se realizează niciun calcul, Prolog doar face ambele expresii identice cu 1+2
    ```
    - operatorul `is` poate lega doar o variabilă, în partea stângă

    ```prolog
    ?- X is 2 + 1.
    X = 3.

    ?- 2 + 1 is Y.
    ERROR: Arguments are not sufficiently instantiated
    
    ?- X =:= 2 + 1.
    ERROR: Arguments are not sufficiently instantiated
     
    ?- X =\= 2 + 1.
    ERROR: Arguments are not sufficiently instantiated
    ```

În al doilea rând, din exemplele de mai sus se deduce și ce tip de egalitate
verifică fiecare dintre acești operatori:

 - `=` verifică dacă cele două părți unifică (modificarea termenului stâng
  determină modificarea temernului drept și viceversa):

  ``` prolog
  ?- X = 2 + 1.
  X = 2+1.
  ```

 - `==` verifică egalitatea sub formă simbolică (practic verifică asemănător cu
  potrivirea șirurilor de caractere):

  ```prolog
  ?- 1 + 2 == 2 + 1.
  false.
  ?- 2 + 1 == 2 + 1.
  true.
  ```

 - `is` forțează evaluarea expresiilor (doar în partea dreapta) pentru a
   verifica egalitatea și face și o eventuală instanțiere (doar în partea
   stângă):

  ```prolog
  ?- X is 2 + 1.
  X = 3.

  ?- 3 is 2 + 1.
  true.

  ?- 3 is 1 + 2.
  true.
  
  ?- 1 + 2 is 2 + 1.
  false.

  ?- 2 + 1 is 2 + 1.
  false.

  ?- 1 + 2 is 3.
  false. 
  ```

 - forțează evaluarea expresiilor de ambele părți pentru a verifica egalitatea
sau inegalitatea lor valorică: `=:=` (egalitate), `=\=` (inegalitate)

  ```prolog

  ?- 3 =:= 2 + 1.
  true.

  ?- 1 + 2 =:= 3.
  true.
  
  ?- 1 + 2 =:= 2 + 1.
  true.

  ?- 3 =:= 3.
  true.
  ```

### Tipuri de date

Am discutat până acum de:

- atomi (constante)
- numbere
- variabile
- termeni compuși (structuri)

#### Liste

Sunt o colecție ordonată de termeni, identificată prin paranteze pătrate.

- Lista vidă: `[]`
- Lista cu elementele a, b, c: `[a, b, c]`
- Lista nevidă: `[Prim | Rest]` – unde variabila `Prim` se leagă
  ([unifică](https://www.swi-prolog.org/pldoc/man?section=glossary#gloss:unify)
  mai bine zis) cu primul element al listei, iar variabila `Rest` cu lista fără
  acest prim element
- Lista care începe cu n elemente `X1, X2, ..., XN` și continuă cu o altă listă
  `Rest`: `[X1, X2, ..., XN | Rest]`

#### Șiruri

O secvență de caractere (un
[string](https://www.swi-prolog.org/pldoc/man?section=string)) va fi înscrisă
între ghilimele (`"`).

```prolog
?- X= "abc", string(X), writeln(X).
abc
X = "abc".
```

### Documentarea predicatelor și a argumentelor

Pentru claritate, convenția pentru antetele predicatelor se scriu sub forma
`predicat/nrArgumente`:

```prolog
predicat(+Arg1, -Arg2, ?Arg3, ..., +ArgN)
```

Pentru a diferenția intrările (`+`) de ieșiri (`-`), se prefixează argumentele
cu indicatori. Acele argumente care pot fi fie intrări, fie ieșiri se prefixează
cu `?`. Instanțierea parametrilor ține de specificarea acestora:

- `Arg1` va fi deja instanțiat atunci când se va încerca satisfacerea unui scop
  care îl are ca premisă pe `predicat`.
- `Arg2` va fi neinstanțiat atunci când se va încerca satisfacerea predicatului.
  - Dacă predicatul este satisfăcut, `Arg2` va fi instanțiat la finalul
    evaluării.
  - Dacă `Arg2` este deja instanțiat la evaluarea predicatului, evaluarea poate
    servi la verificarea corectitudinii argumentului în raport cu semnificația
    predicatului.
    - Următorul exemplu, din laboratoarele următoare, îl folosește pe `R` ca
      intrare, și pe `N` ca o ieșire.
    ```prolog
    % lungime(+Lista,-Lungime)
    lungime([],0).
    lungime([_ | R], N) :- lungime(R, N1), N is N1 + 1.
    % Exemplu, când N este ieșire
    ?- lungime([1, 2, 3], N).
    N = 3.
    % Exemplu, când N este intrare, cu scop de verificare.
    ?- lungime([1, 2, 3], 3).
    true.

    ?- lungime([1, 2, 3], 4).
    false.
    ```
- `Arg3` va putea fi instanțiat sau nu atunci când se va încerca satisfacerea
  predicatului.

## Resurse
-   [Cheatsheet](https://github.com/cs-pub-ro/PP-laboratoare/raw/master/prolog/intro/prolog-cheatsheet-1.pdf)
-   [Schelet](https://ocw.cs.pub.ro/courses/_media/pp/23/laboratoare/prolog/intro-schelet.zip)
-   [Soluții](https://ocw.cs.pub.ro/courses/_media/pp/23/laboratoare/prolog/intro-solutii.zip)

## Referințe

  - [Learn prolog now\!](http://www.learnprolognow.org/ "wikilink")
  - [Logic, Programming, and Prolog](http://www.ida.liu.se/~ulfni53/lpp/bok/bok.pdf "wikilink")
  - [Built-in Predicates](http://www.swi-prolog.org/pldoc/doc_for?object=section%281,%274%27,swi%28%27/doc/Manual/builtin.html%27%29%29 "wikilink")

[^1]: În logica matematică, o formulă este satisfiabilă dacă este adevărată sub
o anumită asociere de valori variabilelor sale.
