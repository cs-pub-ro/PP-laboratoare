# Prolog: Probleme de căutare în spaţiul stărilor

  - Data publicării: 16.05.2023
  - Data ultimei modificări: 16.05.2023

## Obiective

Scopul acestui laborator este învăţarea unor tehnici de rezolvare a problemelor de căutare în spaţiul stărilor.

Aspectele urmărite sunt:

  - metodele de reprezentare a datelor
  - particularităţile diverselor tehnici de rezolvare
  - eficienţa
  - abstractizarea procesului de rezolvare

## Căutare în spaţiul stărilor

Fie un sistem dinamic care se poate afla într-un număr finit de stări. Definim un graf orientat în care nodurile sunt stări, iar arcele reprezintă posibilitatea de a evolua direct dintr-o stare în alta. Graful conține un set de stări iniţiale şi un set de stări finale (scopuri). Problema descrisă de sistem va fi rezolvată prin parcurgerea spaţiului stărilor până când este găsită o cale de la o stare iniţială la o stare finală, în cazul în care problema admite soluţie. Căutarea este un mecanism general, recomandat atunci când o metodă directă de rezolvare a problemei nu este cunoscută.

Exemple de strategii de căutare în spaţiul stărilor:

  - generare şi testare
  - backtracking
  - căutare în adâncime/lăţime
  - algoritmul A\*

## Backtracking atunci când calea către soluţie admite un număr nedeterminat de stări intermediare

În această situaţie nu este posibil să definim un template care descrie forma soluţiei problemei. Vom defini o căutare mai generală, după modelul următor:

```prolog 
solve(Solution):-
    initial_state(State),
    search([State], Solution).
```

`search(+StăriVizitate,-Soluţie)` definește mecanismul general de căutare astfel:

  - căutarea începe de la o stare inițială dată (predicatul `initial_state/1`)
  - dintr-o stare curentă se generează stările următoare posibile (predicatul `next_state/2`)
  - se testează că starea în care s-a trecut este nevizitată anterior (evitând astfel traseele ciclice)
  - căutarea continuă din noua stare, până se întâlneşte o stare finală (predicatul `final_state/1`)

```prolog 
search([CurrentState|Other], Solution):-
    final_state(CurrentState), !,
    reverse([CurrentState|Other], Solution).
    
search([CurrentState|Other], Solution):-
    next_state(CurrentState, NextState),
    \+ member(NextState, Other),
    search([NextState,CurrentState|Other], Solution).

```

## Căutare în lăţime

Căutarea în lăţime este adecvată situaţiilor în care se doreşte drumul
minim între o stare iniţială şi o stare finală. La o căutare în lăţime,
expandarea stărilor "vechi" are prioritate în faţa expandării stărilor
"noi".

```prolog 
do_bfs(Solution):-
    initial_node(StartNode),
    bfs([(StartNode,nil)], [], Discovered),
    extract_path(Discovered, Solution).
```

`bfs(+CoadaStărilorNevizitate,+StăriVizitate,-Soluţie)` va defini mecanismul general de căutare în lăţime astfel:

  - căutarea începe de la o stare inițială dată care n-are predecesor în spaţiul stărilor (StartNode cu părintele nil)
  - se generează toate stările următoare posibile
  - se adaugă toate aceste stări la coada de stări încă nevizitate
  - căutarea continuă din starea aflată la începutul cozii, până se întâlneşte o stare finală

## Căutare A\*

A\* este un algoritm de căutare informată de tipul best-first search, care caută calea de cost minim (distanță, cost, etc.) către scop. Dintre toate stările căutate, o alege pe cea care pare să conducă cel mai repede la soluție. A\* selectează o cale care minimizează `f(n) = g(n)+ h(n)`, unde `n` este nodul curent din cale, `g(n)` este costul de la nodul de start până la nodul `n` și `h(n)` este o euristică ce estimează cel mai mic cost de la nodul `n` la nodul final.

```prolog 
astar_search(Start, End, Grid, Path) :-
    manhattan(Start, End, H),  
    astar(End, [H:Start], [Start:("None", 0)], Grid, Discovered),
    get_path(Start, End, Discovered, [End], Path).
```

`astar(+End, +Frontier, +Discovered, +Grid, -Result)` va defini
mecanismul general de căutare A\*, astfel:

  - căutarea începe de la o stare inițială dată care n-are predecesor în spaţiul stărilor (StartNode cu părintele "None") și distanța estimată de la acesta până la nodul de final printr-o euristică (exemplu: distanța Manhattan)
  - se generează toate stările următoare posibile și se calculează costul acestora adăugând costul acțiunii din părinte până în starea aleasă cu costul real calculat pentru a ajunge în părinte (costul părintelui în Discovered)
  - dacă starea aleasă nu este în Discovered sau dacă noul cost calculat al acesteia este mai mic decât cel din Discovered, se adaugă în acesta, apoi va fi introdusă în coada de priorități (Frontier) cu prioritatea fiind costul cu care a fost adaugată în Discovered + valoarea dată de euristică din starea curentă până în cea finală
  - căutarea continuă din starea aflată la începutul cozii, până se întâlneşte o stare finală


## Resurse


-   [Cheatsheet](https://github.com/cs-pub-ro/PP-laboratoare/raw/master/prolog/cautare/prolog-cheatsheet-3.pdf)
-   [Schelet](https://ocw.cs.pub.ro/courses/_media/pp/23/laboratoare/prolog/cautare-schelet.zip)
-   [Soluții](https://ocw.cs.pub.ro/courses/_media/pp/23/laboratoare/prolog/cautare-solutii.zip)

## Referinţe

  - "Prolog Programming for Artificial Intelligence", Ivan Bratko
