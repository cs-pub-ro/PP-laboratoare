# Prolog: Introducere

## Motivație

### Prolog

Prolog a fost unul dintre
[primele](https://en.wikipedia.org/wiki/Logic_programming#History) limbaje de
programare **logice**. Permite separarea datelor de procesul de inferență,
programele fiind scrise într-un stil **declarativ** și uniform. Impactul
principal l-a avut în cercetare, în domeniul inteligenței artificiale, dar a
rămas un punct de inspirație pentru limbajele de după.

Numele limbajului este o abreviere pentru *programmation en logique*, și este
bazat pe calcul cu predicate.

### Recapitulare teorie

Logica cu predicate de ordin I este o extensie a logicii propoziționale, prin
folosirea de variabile cuantificate pentru a stabili relații. Urmăriți
următoarele două exemple de transformare din logica propozițională în cea cu
predicate de ordin I.

> Toții peștii respiră. (prin branhii)

$$\forall X . (peste(X) \Rightarrow respria(X))$$

> Unii pești au o respirație aeriană. (prin plămâni)

$$\exists X. (peste(X) \land respiraAer(X))$$

Limbajul restricționează această logică doar la folosirea de clauze Horn:

$$\begin{align}
A_1 \land A_2 \land \dots \land A_n &\Rightarrow A \\
true &\Rightarrow B
\end{align}$$

Deci următoarea implicație nu poate fi transcrisă direct într-o regulă în
Prolog, pentru că are ca implicație o disjuncție dintre două predicate.

$$\begin{rcases}
int(a) \\
int(b) \\
a \neq 0 \\
sum(a, b) = 0
\end{rcases}
\Rightarrow negative(a) \lor negative(b)
$$

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

Programele scrise în Prolog descriu relații exprimitate prin clauze. Există două
tipuri de clauze:

- axiome (en. *facts*)
- reguli

"Calculul" modelează în această paradigmă efectuarea de raționamente.

### Axiome

Axiomele sunt predicate de ordinul I de
[aritate](https://ro.wikipedia.org/wiki/Aritate) *n*, considerate **adevărate**.

Exemple de axiome:

```prolog
% Acesta este un comentariu.
% Predicate de aritate 1. (unare)
caine(cerberus).
om(socrate). 
muritor(leulDinNemeea).
muritor(rhesus).

% Predicate de aritate 2. (binare)
% cel_mai_bun_prieten(?Cine, ?AlCui)
cel_mai_bun_prieten(cerberus, hades)

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
Tipuri simpli de termene: constante, sau mai bine zis *atomi* simbolici,
întregi, numere în virgulă mobilă sau termeni compuși.

Cuvântul
[structură](https://www.swi-prolog.org/pldoc/man?section=glossary#gloss:structure)
este un sinonim pentru termen
[compus](https://www.swi-prolog.org/pldoc/man?section=glossary#gloss:compound).

```prolog
% exemplu structură
client(nume(ion, popescu), carte(aventuri, 2002)).
```

Puteți considera momentan că sintactic singura diferență este că predicatele nu
sunt transmise ca argumente, aceasta fiind o
[discuție](https://stackoverflow.com/questions/28972038/prolog-structurecomplex-term-vs-predicate-i-dont-really-get-the-difference)
mai subtilă ce ține de reprezentarea internă a implementării.

Consultați [**glosarul**](https://www.swi-prolog.org/pldoc/man?section=glossary) în caz de orice neînțelegere!!!

### Scopuri și variabile

Când rulăm interogări despre termeni și relațiile dintre ei spunem informal că
demonstrăm sau obținem informații pornind de la "baza noastră de date" (de la
axiome).

Calcul se face prin încercarea de a satisface[^1] *scopuri* (en. *goals*).

**OBSERVAȚIE**: Când am interogat dacă Socrate este muritor, procesul de
execuție a returnat `false` deoarece **nu** se putea satisface acest scop. Nu
înseamnă că el este nemuritor. (*Ipoteza lumii închise*.)

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

### Reguli

```
Antet :- Corp.
```

O regulă este o declarație cu forma generală de mai sus, unde antentul este un predicat, iar corpul este alcătuit din premise separate de operatori. Ea se citește așa:

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
  [operatorul `\+`](https://www.swi-prolog.org/pldoc/doc_for?object=(%5C%2B)/1)
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

Deci mai întâi încearcă demonstrarea primei reguli pentru predicatul `muritor`
și eșuează. Prima premisă din a doua regulă este adevărată (`viu(X)`). Observăm
că **eșecul demonstrației** scopului `zeu(hercule)` determină adevărată a doua
premisă (`\+ zeu(X)`). Cele două premise fiind puse în conjuncție, considerăm că
Hercule este muritor.

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

De asemenea, nu putem să "corectăm" greșeala prin "hardcodarea" valorii `false`,
ca mai jos, întrucât procesul de execuție încearcă în ordinea din fișier toate
declarațiile unui predicat până la satisfacerea acelui scop.

```prolog
muritor(hunabku) :- false. % declarație ineficace
```

### Documentarea predicatelor și a argumentelor

Pentru claritate, antetele predicatelor se scriu sub forma `predicat/nrArgumente`:

```prolog
predicat(+Arg1, -Arg2, ?Arg3, ..., +ArgN)
```

Pentru a diferenția intrările (`+`) de ieșiri (`-`), se prefixează argumentele
cu indicatori. Acele argumente care pot fi fie intrări, fie ieșiri se prefixează
cu `?`. Instanțierea parametrilor ține de specificarea acestora:

- `Arg1` va fi deja instanțiat atunci când se va încerca satisfacerea unui scop
  care îl are ca premisă pe `predicat`.
- `Arg2` va fi neinstanțiat atunci când se va încerca satisfacerea predicatului.
  - Dacă predicatul este satisfăcut, `Arg2` va fa fi instanțiat la finalul
    evaluării.
  - Dacă `Arg2` este deja instanțiat la evaluarea predicatului evaluarea poate
    servi la verificare corectitudinii argumentului în raport cu semnificația
    predicatului.
- `Arg3` va putea fi instanțiat sau nu atunci când se va încerca satisfacerea
  predicatului.

## Resurse
-   [Cheatsheet](https://github.com/cs-pub-ro/PP-laboratoare/raw/master/prolog/intro/prolog-cheatsheet-1.pdf)
-   [Schelet](https://ocw.cs.pub.ro/courses/_media/pp/22/laboratoare/prolog/intro-schelet.zip)
-   [Soluții](https://ocw.cs.pub.ro/courses/_media/pp/22/laboratoare/prolog/intro-solutie.zip)

## Referințe

  - [Learn prolog now\!](http://www.learnprolognow.org/ "wikilink")
  - [Logic, Programming, and Prolog](http://www.ida.liu.se/~ulfni53/lpp/bok/bok.pdf "wikilink")
  - [Built-in Predicates](http://www.swi-prolog.org/pldoc/doc_for?object=section%281,%274%27,swi%28%27/doc/Manual/builtin.html%27%29%29 "wikilink")

[^1]: În logica matematică, o formulă este satisfiabilă dacă este adevărată sub
o anumită asociere de valori variabilelor sale.
