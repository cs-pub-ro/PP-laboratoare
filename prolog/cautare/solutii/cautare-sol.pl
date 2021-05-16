%% -------------------------------------------------------------
%% -------------------------------------------------------------
:- discontiguous exercitiul/2, initial_state/2, final_state/2, next_state/3.

%% -------------------------------------------------------------
%% -------------------------------------------------------------

%% -- BACKTRACKING ATUNCI CÂND NU CUNOAȘTEM LUNGIMEA SOLUȚIEI --
exercitiul(1, []).

%% Problema țăranului, a lupului, a caprei și a verzei.
%%
%% Vom reprezenta o stare astfel:
%% state(MalTaran, Lista-cu-cine-este-pe-malul-cu-taranul)
%%
%% Vom da acestei probleme numele 'taran'.
%% NOTĂ: implementarea se putea face doar cu 2 clauze pentru  next_state

opus(est, vest).
opus(vest, est).

%% TODO
%% safeTaran/1
%% safeTaran(+)
%% Verifică dacă cine rămâne pe vechiul mal este safe
safeTaran :- fail.
safeTaran([]).
safeTaran([_]).
safeTaran(Cine) :- sort(Cine, [lup, varza]).

allTaran([capra, lup, varza]).

initial_state(taran, state(est, Cine)) :- allTaran(Cine).

final_state(taran, state(vest, Cine)) :- allTaran(All), sort(Cine, All).

%% Taranul calatoreste singur
next_state(taran, state(MalTaran1, Cine1), state(MalTaran2, Cine2)) :-
        allTaran(All),
        % pe celălalt mal sunt cei care nu sunt pe malul cu țăranul.
        setMinus(All, Cine1, Cine2),
        % cine rămâne pe vechiul mal este ok.
        safeTaran(Cine1),
        % țăranul merge pe celălalt mal.
        opus(MalTaran1, MalTaran2).

%% Taranul calatoreste cu lupul
next_state(taran, state(MalTaran1, Cine1), state(MalTaran2, Cine2)) :-
        allTaran(All),
        % lupul este pe același mal cu țăranul, inițial.
        member(lup, Cine1),
        % pe celălalt mal sunt cei care nu sunt pe malul cu țăranul.
        setMinus(All, Cine1, Cine2A),
        % pe malul unde ajunge țăranul ajunge și lupul.
        Cine2 = [lup | Cine2A],
        % cine rămâne pe vechiul mal este ok.
        setMinus(All, Cine2, Ramas), safeTaran(Ramas),
        % țăranul merge pe celălalt mal.
        opus(MalTaran1, MalTaran2).

%% Taranul calatoreste cu varza
next_state(taran, state(MalTaran1, Cine1), state(MalTaran2, Cine2)) :-
        allTaran(All),
        % varza este pe același mal cu țăranul, inițial.
        member(varza, Cine1),
        % pe celălalt mal sunt cei care nu sunt pe malul cu țăranul.
        setMinus(All, Cine1, Cine2A),
        % pe malul unde ajunge țăranul ajunge și varza.
        Cine2 = [varza | Cine2A],
        % cine rămâne pe vechiul mal este ok.
        setMinus(All, Cine2, Ramas), safeTaran(Ramas),
        % țăranul merge pe celălalt mal.
        opus(MalTaran1, MalTaran2).

%% Taranul calatoreste cu capra
next_state(taran, state(MalTaran1, Cine1), state(MalTaran2, Cine2)) :-
        allTaran(All),
        % capra este pe același mal cu țăranul, inițial.
        member(capra, Cine1),
        % pe celălalt mal sunt cei care nu sunt pe malul cu țăranul.
        setMinus(All, Cine1, Cine2A),
        % pe malul unde ajunge țăranul ajunge și capra.
        Cine2 = [capra | Cine2A],
        % cine rămâne pe vechiul mal este ok.
        setMinus(All, Cine2, Ramas), safeTaran(Ramas),
        % țăranul merge pe celălalt mal.
        opus(MalTaran1, MalTaran2).

% setPlus(+A, +B, -Result)
% concatenează A și B în Result (Atenție! nu elimină duplicate).
setPlus(A, B, Result) :- append(A, B, Result).

% subSet(+Smaller, +Bigger)
% Verifică dacă setul Smaller este inclus în sau egal cu setul Bigger.
subSet(A, B) :- forall(member(X, A), member(X, B)).

% setMinus(+From, +ToRemove, -Result)
% Produce în result lista elementelor din From care nu sunt în ToRemove.
setMinus(From, ToRemove, Result) :-
        findall(E, (member(E, From), \+ member(E, ToRemove)), Result).

%% Predicatele solve/2 și search/3 sunt folosite pentru
%% rezolvarea unei probleme de căutare în spațiul stărilor. 
%% Fiecare dintre predicate ia ca prim argument problema
%% pe care o rezolvăm.
%% Observațiutilizarea predicatelor initial_state/2, final_state/2 și
%% next_state/3. 

%% search(+Pb, +StariVizitate, -Sol)
search(Pb, [CurrentState|Other], Solution) :-
        final_state(Pb, CurrentState),
        !,
        reverse([CurrentState|Other], Solution).

search(Pb, [CurrentState|Other], Solution) :-
        next_state(Pb, CurrentState, NextState),
        \+ member(NextState, Other),
        search(Pb, [NextState,CurrentState|Other], Solution).

%% solve(+Pb, -Sol)
solve(Pb, Solution):-
        initial_state(Pb, State),
        search(Pb, [State], Solution).

% Vizualizați soluțiile cu
% solve(taran, Sol), validSol(taran, Sol).

%% Problema Misionarilor și Canibalilor
%% ====================================
%% Fie un râu cu două maluri, trei misionari, trei canibali și o
%% barcă. Barca și cei 6 se află inițial pe un mal, iar
%% scopul este ca toți să ajungă pe malul opus. Barca are capacitate de
%% maximum două persoane și nu poate călători fără nicio persoană.
%% Găsiți o secvență de traversări, astfel încât nicăieri să nu existe
%% mai mulți canibali decât misionari (pot exista însă pe un mal doar
%% canibali).
%%
%% Primul pas este definirea unui format pentru starea problemeo. 
%% Ce informații ar trebui să conțină starea? Este suficient să conțină 
%% malul și numărul de canibali, respectiv misionari de pe acesta?
%%
%% Scrieți predicatele initial_state, final_state, și next_state
%% pentru problema misionarilor.

%% Pentru o mai bună structură, implementați întâi predicatele boat/2
%% și safeMisionari/2 detaliate mai jos.

%% Predicate utile: sort/2, @</2 (vedeți help)


% TODO
% boat/2
% boat(-NM, -NC)
% Posibilele combinații de număr de misionari și canibali care pot
% călători cu barca.
boat(_, _) :- fail.
boat(0, 1).
boat(0, 2).
boat(1, 0).
boat(2, 0).
boat(1, 1).

% TODO
% safe/2
% safe(+NM, +NC)
% Verifică dacă numărul dat de misionari și canibali pot fi pe același
% mal.
% Atenție la de câte ori este adevărat safeMisionari pentru diverse
% valori ale argumentelor - poate influența numărul soluțiilor pentru
% problemă.
safeMisionari(_, _) :- fail.
safeMisionari(0, _).
safeMisionari(M, C) :- M > 0, M >= C.

% TODO
% parseState/3
% parseState(+State, -Mal, -NM_Est, -NC_Est, -NM_Vest, -NC_Vest)
% Primește o stare și întoarce în ultimele 5 argumente malul unde este barca 
% și numerele de misionari / canibali de pe malul estic, respectiv vestic, în 
% starea dată.
parseState( _, _, _, _, _, _) :- fail.
parseState(state(est, M, C), est, M, C, OM, OC) :- !,
        OM is 3 - M, OC is 3 - C.
parseState(state(vest, M, C), vest, OM, OC, M, C) :-
        OM is 3 - M, OC is 3 - C.

% TODO
% initial_state(misionari, -State)
% Determină starea inițială pentru problema misionarilor, în formatul
% ales.
initial_state(misionari, _) :- fail.
initial_state(misionari, state(est, 3 ,3)).

% TODO
% final_state(misionari, +State)
% Verifică dacă starea dată este stare finală pentru problema
% misionarilor.
final_state(misionari, _) :- fail.
final_state(misionari, state(vest, 3, 3)).


% TODO
% next_state(misionari, +S1, -S2)
% Produce o stare următoare S2 a stării curente S1.
% Toate soluțiile predicatului next_state pentru o stare S1 dată trebuie
% să fie toate posibilele stări următoare S2 în care se poate ajunge din
% S1.

next_state(misionari, _, _) :- fail.
next_state(misionari, state(Mal1, M1, C1), state(Mal2, M2, C2)) :-
        opus(Mal1, Mal2),
        boat(MB, CB), MB =< M1, CB =< C1,
        M2 is 3 - M1 + MB, C2 is 3 - C1 + CB,
        OM2 is 3 - M2, OC2 is 3 - C2,
        safeMisionari(M2, C2),
        safeMisionari(OM2, OC2).


% dacă solve(misionari, Sol) eșuează, folosiți
% tracksolve(misionari, Sol) pentru a inspecta construcția soluției.

check1 :- tests([
              % a - c
              ckA('boat', [(1, 0), (1, 1), (2, 0)]),
              ech('boat(X, Y)', ['X + Y > 0', '(X >= Y ; X == 0)', 'X + Y =< 2']),
              nsl('boat(X, Y)', 'X/Y', 5),
              % d - h
         0.2, chk(safeMisionari(3, 3)),
         0.2, chk(safeMisionari(3, 2)),
         0.2, chk(safeMisionari(0, 3)),
         0.2, uck(safeMisionari(2, 3)),
         0.2, uck(safeMisionari(1, 3)),
              % i - k
              chk(initial_state(misionari, _)),
              exp('initial_state(misionari, S), parseState(S, M, ME, CE, MV, CV)',
                  ['M', est, 'ME', 3, 'CE', 3, 'MV', 0, 'CV', 0]),
              exp('initial_state(misionari, S), next_state(misionari, S, S1)',
                  [cond('parseState(S1, _, _, _, _, _)')]),
              % l - n
           2, exp('solve(misionari, Sol)', [cond('validSol(misionari, Sol)')]),
           2, ech('solve(misionari, Sol)', ['validSol(misionari, Sol)']),
           2, nsl('solve(misionari, Sol)', 'Sol', 4)
          ]).


%% -------------------------------------------------------------
%% -------------------------------------------------------------
exercitiul(2, []).
%% Parcurgere BFS  grafuri

edge(a,b). edge(a,c). edge(a,d).
edge(c,e). edge(c,f).
edge(d,h).
edge(e,a). edge(e,g).
edge(f,a). edge(f,g).
edge(g,h).

initial_node(a).
final_node(h).


do_bfs(Solution):-
    initial_node(StartNode),
    bfs([(StartNode,nil)], [], Discovered),
    extract_path(Discovered, Solution).

%% TODO
%% Implementați un predicat bfs/3 care să descrie un mecanism de căutare în
%% lățime într-un graf. Se dau predicatele initial_node/1, final_node/1 și
%% edge/2. Observați similaritatea cu initial_state/2, final_state/2 și
%% next_state/2.

%% bfs/3
%% bfs(+Frontier, +Closed, -Solution)
%% Frontier reprezintă coada nodurilor ce vor fi explorate, Closed reprezintă
%% lista nodurilor vizitate deja, iar Solution va reprezenta lista finală a
%% nodurilor vizitate până la găsirea soluției.
%% Toate cele 3 liste vor avea elementele în forma pereche (Nod, Părinte).

bfs([(FinalNode,Parent)|_], Closed, [(FinalNode, Parent)|Closed]):-
        final_node(FinalNode), !.
bfs([(CurrentNode,_)|Rest], Closed, Solution):-
        % nu explorăm noduri care sunt deja închise
        member((CurrentNode,_), Closed),
        !,
        bfs(Rest, Closed, Solution).
bfs([(CurrentNode,Parent)|Rest], Closed, Solution):-
        % găsim toți copiii lui CurrentNode
        findall((Node, CurrentNode), edge(CurrentNode, Node), Children),
        % îi adăugăm la frontieră
        append(Rest, Children, NewFrontier),
        % continuăm
        bfs(NewFrontier, [(CurrentNode,Parent)|Closed], Solution).


%% TODO
%% extract_path/2
%% extract_path(Discovered, Solution)
%% Solution reprezintă calea de la nodul inițial la cel final extrasă din
%% lista nodurilor vizitate (dată sub formă de perechi (Nod, Părinte).

%% Hint: folosiți un predicat auxiliar pentru a construi calea plecând
%% de la nodul final. Pentru fiecare nod căutați părintele lui în Discovered,
%% până ajungeți la nodul inițial.

extract_path(Discovered, Solution):-
        final_node(Node0),
        extract_path(Discovered, [Node0], Solution).

extract_path(Discovered, [Node | Other], [Node | Other]):-
        member((Node,nil), Discovered), !.

extract_path(Discovered, [Node | Other], Solution):-
        member((Node,Next), Discovered),
        extract_path(Discovered, [Next, Node | Other], Solution).

check2:- tests([
            exp("bfs([(a,nil)], [], R)", [
                set('R', [(h, d), (f, c), (e, c), (d, a), (c, a), (b, a), (a, nil)])]),
            exp("extract_path([(h, d), (f, c), (e, c), (d, a), (c, a), (b, a), (a, nil)], R)", [
                set('R', [a, d, h])])
        ]).

%% -------------------------------------------------------------
%% -------------------------------------------------------------
%% Arbori BONUS

exercitiul(3, []).

%% Se dau următoarele fapte ce descriu arcele unei păduri de arbori
%% ATENȚIE: Fiecare nod poate avea acum oricâți copii.

nod(a). nod(b). nod(c). nod(d). nod(e). nod(f). nod(g).
nod(h). nod(i). nod(j). nod(k). nod(l).
nod(m).
nod(n). nod(o). nod(p). nod(q). nod(r). nod(s). nod(t). nod(u). nod(v).

arc(a, [b, c, d]). arc(c, [e, g]). arc(e, [f]).
arc(h, [i]). arc(i, [j, k, l]).
arc(n, [o, p]). arc(o, [q, r, s]). arc(p, [t, u, v]).

% TODO
% preorder/2
% preorder(+N, -Parc)
% Întoarce în Parc o listă de noduri rezultate din parcurgerea în
% preordine începând din noudl N.
preorder(_, _) :- fail.
preorder(_, _) :- fail.
preorder(N, Parc) :- parc([N], Parc).

parc([], []).
parc([N | Rest], [N | Parc]) :- \+ arc(N, _), parc(Rest, Parc).
parc([N | Rest], [N | Parc]) :- arc(N, Children),
        append(Children, Rest, L),
        parc(L, Parc).

check3 :- tests([
          exp('preorder(a, P)', ['P', [a, b, c, e, f, g, d]]),
          exp('preorder(n, P)', ['P', [n, o, q, r, s, p, t, u, v]])
          ]).


exercitiul(4, [2, puncte]).
% Dată fiind funcția nodes, parcurgeți toată pădurea de arbori.

% nodes(-NN)
% Întoarce în NN toate nodurile din pădurea de arbori.
nodes(NN) :- findall(N, nod(N), NN).

% TODO
% trees/1
% trees(-Trees)
% Întoarce în trees o listă în care fiecare element este parcurgerea
% unui arbore.
trees(_) :- fail.
trees(Trees) :- nodes(NN), parcAll(NN, Trees).
parcAll([], []).
parcAll([N | Todo], [ParcN | ParcRest]) :-
        preorder(N, ParcN),
        setMinus(Todo, ParcN, Rest),
        parcAll(Rest, ParcRest).

check4 :- tests([
              exp('trees(TT)', ['TT',
                                [[a,b,c,e,f,g,d],[h,i,j,k,l],[m],[n,o,q,r,s,p,t,u,v]]])
          ]).
