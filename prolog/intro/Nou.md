# Prolog: Introducere

## Prolog vs. SWI-Prolog

Prolog a fost unul dintre
[primele](https://en.wikipedia.org/wiki/Logic_programming#History) limbaje de
programare **logice**. E **declarativ**. TODO bla bla a inspirat alte limbaje
utile

### Recapitulare teorie

Logica cu predicate de ordin I

Bazat pe logica cu predicate de ordin I, restricționaționată doar la folosirea
de clauze Horn:

$$\begin{align}
A_1 \land A_2 \land \dots \land A_n &\Rightarrow A \\
true &\Rightarrow B
\end{align}$$

pe scurt nu e voie cu 

$$ \begin{rcases}
int(a) \\
int(b) \\
a \neq 0 \\
sum(a, b) = 0
\end{rcases}
\Rightarrow negative(a) \lor negative(b)
$$

Numele limbajului este o abserviere pentru *programmation en logique*.

SWI-Prolog este o [implementare](https://en.wikipedia.org/wiki/Prolog#ISO_Prolog) a limbajului, având cele mai multe featureuri. TODO
https://en.wikipedia.org/wiki/Comparison_of_Prolog_implementations

Urmăriți instrucțiunile de [aici](..:..:limbaje#prolog "wikilink") pentru TODO 


##  TODO

Programele scrise în Prolog descriu relații definite în termeni de clauze. Există două tipuri de clauze:

  * axiome
  * reguli
  * întrebări, numite și **scopuri**, despre obiecte și relațiile dintre ele

## Axiome

Axiome sunt predicate de ordinul I de
[aritate](https://ro.wikipedia.org/wiki/Aritate) *n*, considerate **adevărate**.

Exemple de axiome:

```prolog
caine(zdreanta).

om(socrate). 
muritor(zdreanta).

TODO axioma cu aritate 3
```

Rulăm următoarele interogări:

```prolog
?- caine(zdreanta).
true.

?- muritor(socrate).
false.
```

**OBSERVAȚIE**: Când am interogat dacă Socrate este muritor, procesul de
execuție a returnat fals deoarece nu a găsit nicio faptă și nicio regulă care să
demonstreze adevărată interogarea. *Ipoteza lumii închise* va fi un subiect pe
care îl vom mai atinge.

## Reguli

```
Antet :- Corp.
```

O regulă este o declarație cu forma generală de mai sus, unde antentul este un predicat, iar corpul este alcătuit din premise separate de operatori. Ea se citește așa:

> Antetul este adevărat dacă corpul este adevărat

Practic este o implicație de la dreapta la stânga.

Exemple:

```prolog
om(socrate) :- true.
om(socrate) :- true.
% Echivalent cu: muritor(socrate).

viu(hercule).   % semi-zeu, nici om, nici zeu
viu(zeus).      % pentru Socrate și Sisif, Zeus era viu
viu(hunabku).   % tatăl zeilor în mitologia mayașă 

zeu(zeus).      % regele zeilor de pe Muntele Olimp

muritor(X) :- om(X).
muritor(X) :- viu(X), \+ zeu(X).
```


Observați că:

* am trecut de la declararea doar prin axiome, folosind două reguli în pentru
  predicatului `muritor(?Cine)`
* am folosit operatorul `,`, "și" logic ($\land$)
* am folosit
  [operatorul `\+`](https://www.swi-prolog.org/pldoc/doc_for?object=(%5C%2B)/1)
  pe care îl puteți trata ca pe o negație pentru moment.



Rulăm următoarele interogări:

```prolog
?- muritor(socrate).
true .

?- muritor(zeus).
false.
```

### Procesul de execuție

Ca să începem să înțelegem cum se execută o întrebare, mai bine zis se încearcă
*satisfacerea unui un scop*, activăm modul trace.

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
premisă (`\+ zeu(X)`). Cele două premise fiind puse în conjucție, se poate că
Hercule este muritor.

### Negația ca eșec în demonstrație

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


În cazul lui Hunabku satisfacerea primei premise cât și eșecul demonstrației că
este zeu, ne determină să îl considerăm muritor. *Totuși* știm că zeii sunt
nemuritori, deci de unde contradicția?!

Întrucât grecii l-au considerat pe Hunabku un personaj influent din zvonuri, nu
un zeu, nu se poate satisface scopul `zeu(hunabku)`. Deci folosirea operatorul
`\+` nu ne-a ajutat, întrucât el **întoarce adevărat dacă nu se poate satisface
argumentul.** (Am încercat să negăm logic un scop pe care nu îl putem
satisface.)