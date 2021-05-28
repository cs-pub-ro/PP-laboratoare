%% -------------------------------------------------------------
%% -------------------------------------------------------------
:- discontiguous exercitiul/2, initial_state/2, final_state/2, next_state/3.

%% -------------------------------------------------------------
%% -------------------------------------------------------------

%% -- BACKTRACKING ATUNCI CÂND NU CUNOAȘTEM LUNGIMEA SOLUȚIEI --
exercitiul(0, []).

%% Problema țăranului, a lupului, a caprei și a verzei.
%% Un țăran, ducând la târg un lup, o capră şi o varză ajunge
%% în dreptul unui râu pe care trebuie să-l treacă. Cum va proceda el, ştiind că:
%% - lupul mănâncă capra şi capra mănâncă varza;
%% - el nu poate să-i treacă pe toţi o dată şi nici câte doi;
%% Vom reprezenta o stare astfel:
%% state(MalTaran, Lista-cu-cine-este-pe-malul-cu-taranul)
%%
%% Vom da acestei probleme numele 'taran'.
%% NOTĂ: implementarea se putea face doar cu 2 clauze pentru  next_state

opus(est, vest).
opus(vest, est).

%% TODO
%% safeTaran/1
%% safeTaran(+Lista-cu-cine-este-pe-malul-opus-taranului)
%% Verifică dacă cine rămâne pe malul opus este în siguranță
%% De exemplu o lista formată din [lup, capra] nu respectă
%% constrângerea setată de problemă
safeTaran(_) :- false.
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

%% Țăranul călătorește cu lupul
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

%% Țăranul călătorește cu varza
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

%% Țăranul călătorește cu capra
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
% https://www.swi-prolog.org/pldoc/man?predicate=subset/2
subSet(A, B) :- subset(A, B).

% setMinus(+From, +ToRemove, -Result)
% Produce în result lista elementelor din From care nu sunt în ToRemove.
% https://www.swi-prolog.org/pldoc/doc_for?object=subtract/3
setMinus(From, ToRemove, Result) :- subtract(From, ToRemove, Result).

%% Predicatele solve/2 și search/3 sunt folosite pentru
%% rezolvarea unei probleme de căutare în spațiul stărilor.
%% Fiecare dintre predicate ia ca prim argument problema
%% pe care o rezolvăm.
%% Observați utilizarea predicatelor initial_state/2, final_state/2 și
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


check0 :- tests([
              chk(safeTaran([])),
              chk(safeTaran([lup])),
              chk(safeTaran([capra])),
              chk(safeTaran([varza])),
              exp('S = [_, _], safeTaran(S)',
                  [set('S', ['lup', 'varza'])]),
              uck(safeTaran([_, _, _]))
        ]).


exercitiul(1, []).

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
%% Primul pas este definirea unui format pentru starea problemei.
%% Pentru a reprezenta malul puteți folosi constantele est si vest
%% folosite anterior.
%% Ce informații ar trebui să conțină starea? Este suficient să conțină
%% malul și numărul de canibali, respectiv misionari de pe acesta?
%%
%% Scrieți predicatele initial_state, final_state, și next_state
%% pentru problema misionarilor.
%%
%% Pentru o mai bună structură, implementați întâi predicatele boat/2
%% și safeMisionari/2 detaliate mai jos.
%%
%% Predicate utile: sort/2, @</2 (vedeți help)


% TODO
% boat/2
% boat(-NM, -NC)
% Posibilele combinații de număr de misionari și canibali care pot
% călători cu barca, unde NM este numărul de misionari, iar NC numărul
% de canibali din bancă
%
% Nu uitați ca barca are capacitate de maximum două persoane și nu poate
% călători fără nicio persoană.
% Ex boat(2, 0)
boat(_, _) :- false.
boat(0, 1).
boat(0, 2).
boat(1, 0).
boat(2, 0).
boat(1, 1).

% TODO
% safe/2
% safe(+NM, +NC)
% Verifică dacă numărul dat de misionari și canibali pot fi pe același
% mal (să nu existe mai mulți canibali decât misionari)
% Atenție la de câte ori este adevărat safeMisionari pentru diverse
% valori ale argumentelor - poate influența numărul soluțiilor pentru
% problemă.
safeMisionari(_, _) :- false.
safeMisionari(0, _).
safeMisionari(M, C) :- M > 0, M >= C.

% TODO
% parseState/6
% parseState(+State, -Mal, -NM_Est, -NC_Est, -NM_Vest, -NC_Vest)
% Primește o stare și întoarce în ultimele 5 argumente malul unde este barca
% și numerele de misionari / canibali de pe malul estic, respectiv vestic, în
% starea dată.
parseState( _, _, _, _, _, _) :- fail.
parseState(Mal/ME/CE/MV/CV, Mal, ME, CE, MV, CV).

% TODO
% initial_state(misionari, -State)
% Determină starea inițială pentru problema misionarilor, în formatul
% ales.
% Hint Barca și cei 6(3 canibali, 3 misionari) se află inițial pe malul estic
initial_state(misionari, _) :- false.
initial_state(misionari,est/3/3/0/0).

% TODO
% final_state(misionari, +State)
% Verifică dacă starea dată este stare finală pentru problema
% misionarilor.
% Hint Barca și cei 6(3 canibali, 3 misionari) se află pe malul vestic
final_state(misionari, _) :- false.
final_state(misionari, vest/0/0/3/3).


% TODO
% next_state(misionari, +S1, -S2)
% Produce o stare următoare S2 a stării curente S1.
% Toate soluțiile predicatului next_state pentru o stare S1 dată trebuie
% să fie toate posibilele stări următoare S2 în care se poate ajunge din
% S1.
% Hinturi
%   - Consecință - malul se schimba (folosiți predicatul opus pentru validare)
%   - Tranziția dintr-o stare în alta se realizează printr-o traversare
%     validă cu barca (folosiți predicatul boat)
%   - Atât pe malul estic cât și cel vestic constrângerea este respectată, și anume
%     nu există mai mulți canibali decât misionari (folosiți predicatul safeMisionari
%     pentru validare). Pentru a calcula numărul de canibali/misionari de pe malul opus
%     ce formulă puteți folosi știind ca numărul total de canibali/misionari este 3?

next_state(misionari, _, _) :- false.
next_state(misionari, est/ME/CE/MV/CV, vest/ME2/CE2/MV2/CV2) :-
        boat(MB, CB), MB =< ME, CB =< CE,
        ME2 is ME - MB, CE2 is CE - CB,
        MV2 is MV + MB, CV2 is CV + CB,
        safeMisionari(ME2, CE2),
        safeMisionari(MV2, CV2).
next_state(misionari, vest/ME/CE/MV/CV, est/ME2/CE2/MV2/CV2) :-
        boat(MB, CB), MB =< MV, CB =< CV,
        ME2 is ME + MB, CE2 is CE + CB,
        MV2 is MV - MB, CV2 is CV - CB,
        safeMisionari(ME2, CE2),
        safeMisionari(MV2, CV2).


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
%% Frontier reprezintă coada nodurilor ce vor fi explorate(nevizitate), Closed reprezintă
%% lista nodurilor vizitate deja, iar Solution va reprezenta lista finală a
%% nodurilor vizitate până la găsirea soluției.
%% Toate cele 3 liste vor avea elementele în forma pereche (Nod, Părinte).
%%
%% Pași de urmat
%% Căutarea începe de la nodul inițial dat (a) care n-are predecesor
%% Se generează apoi toate nodurile accesibile din nodul curent (exista
%%      un arc de la nod la vecin). Folosiți predicatul getNeighb definit mai jos
%%      pentru a genera nodurile vecine
%% Se adaugă toate aceste noduri la coada(lista) de stări încă nevizitate - Frontier
%% Căutarea continuă din starea aflată la începutul frontierei, până se întâlneşte
%%      o stare finală (am ajuns la nodul final dat - h)

% Întoarce în  Result nodurile vecine ale nodului X primit ca parametru
% Exemplu utilizare: getNeighb(a, [], Result).
getNeighb(X, Acc, Result) :- edge(X,Y), \+ memberchk((Y,_), Acc), !, getNeighb(X, [(Y,X)|Acc], Result).
getNeighb(_, Acc, Result) :- reverse(Acc, Result).

bfs(_,_,_) :- false.

bfs([(FinalNode,Parent)|_], Closed, [(FinalNode, Parent)|Closed]):-
        final_node(FinalNode), !.
bfs([(CurrentNode,_)|Rest], Closed, Solution):-
        % nu explorăm noduri care sunt deja închise
        member((CurrentNode,_), Closed),
        !,
        bfs(Rest, Closed, Solution).
bfs([(CurrentNode,Parent)|Rest], Closed, Solution):-
        % găsim toți copiii lui CurrentNode
        getNeighb(CurrentNode, [], Children),
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
% preordine începând din nodul N.
% Hint - definiți un predicat auxiliar care primește lista de noduri
% de vizitat (de forma [N|Rest]) și Parc(listă de noduri rezultate din parcurgere)
% Folosind predicatul arc generați noduri copil ale nodului curent și adăugați în
% restul listei de parcurs (Rest). Folosiți apoi lista rezultat pentru  a genera restul
% listei soluție

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

% nodes(+Acc, -Result)
% Întoarce în Result toate nodurile din pădurea de arbori.
nodes(Acc, Result) :- nod(N), \+ memberchk(N, Acc), !, nodes([N|Acc], Result).
nodes(Acc, Result) :- reverse(Acc, Result).

% TODO
% trees/1
% trees(-Trees)
% Întoarce în trees o listă în care fiecare element este parcurgerea
% unui arbore.
% Folosiți predicatul nodes pentru a  obtine toate nodurile din pădurea
% de arbori. Pentru fiecare nod generați o parcurgere folosind predicatul
% definit anterior. Eliminați folosind setMinus nodurile din NN care apar
% în parcurgerea curentă.
trees(_) :- false.
trees(Trees) :- nodes([], NN), parcAll(NN, Trees).

parcAll([], []).
parcAll([N | Todo], [ParcN | ParcRest]) :-
        preorder(N, ParcN),
        setMinus(Todo, ParcN, Rest),
        parcAll(Rest, ParcRest).

check4 :- tests([
              exp('trees(TT)', ['TT',
                                [[a,b,c,e,f,g,d],[h,i,j,k,l],[m],[n,o,q,r,s,p,t,u,v]]])
          ]).

%% --------------------------------------------
%% teste specifice pentru problemele de căutare
%% --------------------------------------------

err(Sol, Msg, Value, Pb) :-
        format('~n~n Solutia: ~n'),
        (   is_list(Sol) -> forall(member(E, Sol), format('~w~n', [E]))),
        format('~w: ~w (in ~w).~n', [Msg, Value, Pb]), fail.

tracksolve(Pb, Solution) :-
        write('====================================================='), nl,
        write('====================================================='), nl,
        write('====================================================='), nl,
        initial_state(Pb, State),
        printState(Pb, State),
        tracksearch(Pb, [State], Solution).

tracksearch(Pb, [CurrentState|Other], Solution) :-
        final_state(Pb, CurrentState),
        !,
        write('DONE.'), nl,
        reverse([CurrentState|Other], Solution).
tracksearch(Pb, [CurrentState|Other], Solution) :-
        format('Finding next state from ~w ... ', [CurrentState]),
        next_state(Pb, CurrentState, NextState),
        format('Try state: ~w ...', [NextState]),
        trackmember(NextState, Other),
        reverse([NextState,CurrentState|Other], Prog),
        print_progress(Pb, Prog),
        tracksearch(Pb, [NextState,CurrentState|Other], Solution).
tracksearch(_,  [CurrentState|_], _) :-
        format('No other next state found from ~w ...~n ', [CurrentState]),
        fail.

trackmember(State, Other) :- \+ member(State, Other), !, write('continue...'), nl.
trackmember(_, _) :- write('already visited'), nl, nl, fail.

print_progress(_, [_]).
print_progress(Pb, [S1, S2 | Rest]) :-
        printState(Pb, S1),
        printTransition(Pb, S1, S2),
        printState(Pb, S2), !,
        writeln(""),
        print_progress(Pb, [S2 | Rest]).
print_progress(Pb, [S1, S2 | States]) :-
        err([S1, S2 | States],
            "INTERN: caz invalid print_progress", (S1, S2), Pb).

printTransition(taran, state(M1, Elems1), state(_, Elems2)) :-
        allTaran(All), setMinus(All, Elems1, Others),
        setMinus(Elems2, Others, Boat),
        (   Boat = [InBoat], !; Boat = [], InBoat = "-"),
        (   M1 == est, !, format('~17| taran + ~w -> ~n', [InBoat])
        ;   M1 == vest, format('~17| <- taran + ~w ~n', [InBoat])
        ), !.
printTransition(misionari, S1, S2) :-
        parseState(S1, M1, ME1, CE1, _, _),
        parseState(S2, _, ME2, CE2, _, _),
        M1 == est, !,
        MB is ME1 - ME2, CB is CE1 - CE2,
        format('~10| ~w M  ~w C ->~n', [MB, CB]).
printTransition(misionari, S1, S2) :-
        parseState(S1, M1, _, _, MV1, CV1),
        parseState(S2, _, _, _, MV2, CV2),
        M1 == vest, !,
        MB is MV1 - MV2, CB is CV1 - CV2,
        format('~10| <- ~w M  ~w C~n', [MB, CB]).
printTransition(Pb, S1, S2) :-
        err([], "INTERN: caz invalid printTransition", (S1, S2), Pb).

printState(taran, state(Mal, Elems)) :-
        allTaran(All), setMinus(All, Elems, Others),
        (   Mal == est, !,
            format('~w ~20| v~~~~ ~35| ~w ~n', [Elems, Others])
        ;
            Mal == vest,
            format('~w ~20| ~~~~v ~35| ~w ~n', [Others, Elems])
        ), !.
printState(misionari, State) :-
        parseState(State, est, ME, CE, MV, CV), !,
        format('~w M ~w C ~15| v~~~~ ~25| ~w M ~w C~n', [ME, CE, MV, CV]).
printState(misionari, State) :-
        parseState(State, vest, ME, CE, MV, CV), !,
        format('~w M ~w C ~15| ~~~~v ~25| ~w M ~w C~n', [ME, CE, MV, CV]).
printState(Pb, State) :-
        err([], "INTERN: caz invalid printState", State, Pb).

validSol(Pb, Sol) :- %format('~n~n Solutia: ~n'),
        (   \+ is_list(Sol), !, err(Sol, 'Solutia nu este o lista', Sol, Pb)
        ;   !, forall(member(E, Sol), validState(Pb, Sol, E))
        , validTransitions(Pb, Sol, Sol), last(Sol, Last), validFinal(Pb, Sol, Last)).
validSol(Pb, Sol) :- format('INTERN: caz invalid validSol ~w: ~w~n', [Pb, Sol]), fail.

validMal(_, _, _, 0, _) :- !.
validMal(_, _, _, M, C) :- M >= C, !.
validMal(Sol, S, Mal, M, C) :-
        swritef(Msg, 'Numar incorect de misionari/canibali %w/%w pe malul %wic in starea',
               [M, C, Mal]),
        err(Sol, Msg, S, misionari).
validState(taran, Sol, S) :- allTaran(All),
        (   S = state(Mal, Elements),
            (   \+ member(Mal, [est, vest]), err(Sol, 'Mal invalid', Mal, taran)
            ;   (\+ subSet(Elements, All), setMinus(Elements, All, Inv),
                err(Sol, 'Elemente invalide', Inv, taran)
                ;   setMinus(All, Elements, OtherSide), validOther(Sol, OtherSide)))
        ;   err(Sol, 'Stare in format incorect', S, taran)).
validState(misionari, Sol, S) :-
        (   parseState(S, M, ME, CE, MV, CV), !,
            (   member(M, [est, vest]), !,
                (   ME + MV =:= 3, CE + CV =:= 3, !,
                    validMal(Sol, S, est, ME, CE), validMal(Sol, S, vest, MV, CV)
                ;   err(Sol, 'numar incorect de persoane in stare', S, misionari)
                )
            ;   err(Sol, 'mal incorect in stare', M/S, misionari)
            )
        ;   err(Sol, 'parseState a esuat pentru', S, misionari)
        ).
validState(Pb, Sol, S) :- err(Sol, 'INTERN: caz invalid validState', S, Pb).
validOther(_, []) :- !.
validOther(_, [_]) :- !.
validOther(_, L) :- sort(L, [lup, varza]), !.
validOther(Sol, L) :- member(lup, L), member(capra, L), !,
        err(Sol, 'Lupul mananca capra', L, taran).
validOther(Sol, L) :- member(varza, L), member(capra, L), !,
        err(Sol, 'Capra mananca varza', L, taran).
validOther(Sol, L) :- err(Sol, 'INTERN: caz invalid validOthers', L, taran).
validTransitions(_, _, [_]) :- !.
validTransitions(taran, Sol, [S1, S2 | Rest]) :- !,
        S1 = state(Mal1, _Cine1), S2 = state(Mal2, _Cine2),
        (   \+ opus(Mal1, Mal2), !, err(Sol, 'nu sunt opuse:', [Mal1, Mal2], taran)
        ;   validTransitions(taran, Sol, [S2 | Rest])). % not fully checked
validTransitions(misionari, Sol, [S1, S2 | Rest]) :- !,
        parseState(S1, Mal1, ME1, CE1, MV1, CV1), parseState(S2, Mal2, ME2, CE2, MV2, CV2),
        (   \+ opus(Mal1, Mal2), !, err(Sol, 'nu sunt opuse:', [Mal1, Mal2], misionari)
        ;
        (   Mal1 == est, !, validBoat(misionari, Sol, S1, S2, ME1, CE1, ME2, CE2)
        ;   validBoat(misionari, Sol, S1, S2, MV1, CV1, MV2, CV2)
        )), validTransitions(misionari, Sol, [S2 | Rest]).
validTransitions(Pb, Sol, Rest) :- err(Sol, 'INTERN: caz invalid validTransitions', Rest, Pb).
validBoat(misionari, Sol, S1, S2, M1, C1, M2, C2) :-
        MB is M1 - M2, CB is C1 - C2,
        (   MB + CB > 0, MB + CB =< 2, (MB == 0, !; MB >= CB), !
        ;   swritef(Msg, 'numar incorect de misionari/canibali %w/%w in barca intre starile',
                    [MB, CB]), err(Sol, Msg, S1/S2, misionari)
        ).
validFinal(taran, Sol, state(Mal, Elements)) :- allTaran(All),
        (   Mal \= vest, !, err(Sol, 'Nu ajunge pe malul vestic la sfarsit', Mal, taran)
        ;   ( \+ sort(Elements, All), setMinus(All, Elements, Miss),
            !, err(Sol, 'Nu au ajuns', Miss/Elements, taran), fail
            ; !, true)).
validFinal(misionari, Sol, S) :- parseState(S, Mal, _, _, MV, CV),
        (   Mal \= vest, !, err(Sol, 'Nu ajunge pe malul vestic la sfarsit', Mal, misionari)
        ;   ( MV \= 3, CV \= 3, !, err(Sol, 'Au ajuns doar', MV/CV, misionari), fail
            ; !, true)).
validFinal(Pb, Sol, S) :- err(Sol, 'INTERN: caz invalid validFinal', S, Pb).


%% ----------------------------------------
%% ----------------------------------------
%% Tester

% pentru vmchecker, trebuie pentru fiecare segment de testare să existe:
% o afirmație vmpoints(<ID_segment>, <Număr_puncte_segment>)
% o afirmație tt(<ID_segment>, <Listă_teste>)
% Trebuie ca test_mode(vmchecker) să fie adevărat.

% pentru quickchecking (la laborator), trebuie ca pentru fiecare
% exercițiu să existe:
% o afirmație exercitiul(<ID>, [<Număr puncte>, alte, comentarii])
% o afirmație check<ID>(tests(<Listă_teste>))
% e.g. dacă există exercitiul(5), trebuie să existe și check5(...)
%
% Tipurile de teste sunt prezentate în tester.pl, în cadrul predicatului
% testtest/0.

testtimelimit(5). % in seconds

%test_points(show). % uncomment to show points in non-vmchecker mode.
test_points(hide) :- test_mode(vmchecker); \+ test_points(show).

%test_mode(vmchecker). % uncomment to activate the vmchecker mode.
test_mode(quickcheck) :- \+ test_mode(vmchecker).

:-dynamic(punct/2).
:-dynamic(current/1).
%:-clean.

clean :- retractall(punct(_, _)), retractall(current(_)).

% -----------------

% runs quickcheck tests
check :-
        clean,
        forall(exercitiul(Ex,_),
               (   atom_concat(check, Ex, Ck),
                   retractall(current(_)), assert(current(Ex)),
                   once(call(Ck)) ; true)),
        findall(P, punct(_, P), L),
        sum_list(L, S),
        (   test_points(show),
            format('Punctaj total: ~f~n',[S])
        ;   true),
        clean.

% entry point for quick check; handles checking all exercises or just
% one.
tests(Tests) :- (   current(_), ! ; retractall(punct(_, _))),
        (   current(Ex), !, (exercitiul(Ex, [Pts | _]), !, Total is Pts
                            ;
                            exercitiul(Ex, []), Total is 0)
        ;   Total is 100, Ex = none
        ),
        tests(Tests, Total, Ex, Score),
        (   current(Ex), assert(punct(Ex, Score)), !
        ;   format('Rezolvat ~0f%.~n', [Score])
        ), !.
tests(_) :- failure(unknown, 'INTERN: tests/1 failed').


% ---------------
% general testing

% unified entry point for testing; computes fractions, computes if
% exercise is not done, and starts per-test iteration.
tests(Tests, TotalPoints, Ex, Score) :- %trace,
    total_units(Tests, TF, Ck/AllCheck, UCk/AllUCk, Others/AllOthers),
    %format('Total units: ~w~n', [TF]),
    (   isNotDone(Ck/AllCheck, UCk/AllUCk, Others/AllOthers), !,
        (   Ex == none, !
        ;   ( test_mode(vmchecker), !, format("+0.00 ~10t  ") ; true ),
            format("[~w] Nerezolvat.~n", [Ex])
        ),
        Score = 0
    ;   Unit is TotalPoints / TF,
        tests(Tests, Ex, 1, Unit, 0, Score)
    ), !.
tests(_, _, Ex, _) :- failure(Ex, 'INTERN: tests/4 failed').

isNotDone(0/TC, TU/TU, 0/TO) :- (TO > 0, !; TC > 0).
% otherwise, probably done

% iterates through tests, handles test index, generates test id, adds
% points
tests([], _, _, _, Points, Points) :- !.
tests([wait|R], Ex, Idx, Unit, PointsIn, PointsOut) :- !,
    tests(R, Ex, Idx, Unit, PointsIn, PointsOut).
tests([Fraction, T|R], Ex, Idx, Unit, PointsIn, PointsOut) :-
        number(Fraction), !, test(T, Ex, Idx, Fraction, Unit, PointsIn, PointsOut1),
        tests(R, Ex, Idx+1, Unit, PointsOut1, PointsOut).
tests([T|R], Ex, Idx, Unit, PointsIn, PointsOut) :-
        test(T, Ex, Idx, 1, Unit, PointsIn, PointsOut1),
        tests(R, Ex, Idx+1, Unit, PointsOut1, PointsOut).
tests(_, Ex, _, _, _, _) :- failure(Ex, 'INTERN: tests/6 failed').

total_units([], 0, 0/0, 0/0, 0/0).
total_units([wait, P, _|R], Tot, A, B, C) :-
    number(P), !, total_units([counted|R], TotR, A, B, C), Tot is TotR + P.
total_units([wait, _|R], Tot, CO/TCO, UO/TUO, OO/TOO) :- !,
    total_units(R, TotR, CO/TCO, UO/TUO, OO/TOO), Tot is TotR + 1.
total_units([P, T|R], Tot, A, B, C) :-
    number(P), !, total_units([counted, T|R], TotR, A, B, C), Tot is TotR + P.
total_units([counted, T|R], Tot, CO/TCO, UO/TUO, OO/TOO) :- !, %trace,
    test(T, dry, dry, _, _, 0, P),
    (   ( T = chk(_), ! ; T = ckA(_, _) ), !, TA = 1,
        (   P > 0, A = 1, !; A = 0 )
    ;   TA = 0, A = 0),
    (   ( T = uck(_), ! ; T = nsl(_, _, 0) ), !, TB = 1,
        (   P > 0, B = 1, !; B = 0 )
    ;   TB = 0, B = 0),
    (   T \= chk(_), T \= ckA(_, _), T \= uck(_), T \= ech(_, _), T \= nsl(_, _, 0), !,
        TD = 1, (   P > 0, D = 1, !; D = 0 )
    ;   TD = 0, D = 0),
    total_units(R, TotR, C/TC, U/TU, O/TO), Tot is TotR,
    CO is C+A, TCO is TC+TA, UO is U+B, TUO is TU+TB, OO is O+D, TOO is TO+TD.
total_units(TT, Tot, A, B, C) :-
    !, total_units([counted|TT], TotR, A, B, C), Tot is TotR + 1.

test(T, NEx, Idx, Fraction, Unit, PointsIn, PointsOut) :-
        (   NEx == dry, !, Ex = dry, TimeLimit = 0.1
        ;   testtimelimit(TimeLimit),
            IdxI is Idx + 96, char_code(CEx, IdxI),
            (   NEx == none, !, swritef(Ex, '%w|', [CEx])
            ;   swritef(Ex, '[%w|%w]', [NEx, CEx]))
        ),
        swritef(MTime, 'limita de %w secunde depasita', [TimeLimit]),
        (   catch(
                catch(call_with_time_limit(TimeLimit, once(test(Ex, T))),
                      time_limit_exceeded,
                      except(Ex, MTime)
                     ),
                Expt,
                (   swritef(M, 'exceptie: %w', [Expt]), except(Ex, M))
            ),
            !, success(Ex, Fraction, Unit, Points),
            PointsOut is PointsIn + Points
        ; PointsOut = PointsIn).
test(_, Ex, Idx, _, _, _, _) :- failure(Ex/Idx, 'INTERN: test/7 failed').

success(dry, _, _, 1) :- !.
success(Ex, Fraction, Unit, Score) :-
    Score is Fraction * Unit,
    %format('~w ~w ~w ~w~n', [Ex, Fraction, Unit, Score]),
    (   test_mode(vmchecker), !,
        format('+~2f ~10t  ~w Corect.~n', [Score, Ex])
    ;
    (   test_points(show),
        format('~w[OK] Corect. +~2f.~n', [Ex, Score])
    ;   format('~w[OK] Corect.~n', [Ex])
    )).
failure(dry, _) :- !, fail.
failure(Ex, M) :-
        (   test_mode(vmchecker), !,
            format('+0.00 ~10t  ~w ~w~n', [Ex, M]), fail
        ;   format('~w[--] ~w~n', [Ex, M]), fail).
except(dry, _) :- !, fail.
except(Ex, M) :-
        (   test_mode(vmchecker), !,
            format('+0.00 ~10t ~w Exception: ~w~n', [Ex, M]), fail
        ;   format('~w[/-] ~w~n', [Ex, M]), fail).

test(Ex, chk(P)) :- !, testCall(Ex, P).
test(Ex, uck(P)) :- !, testCall(Ex, \+ P).
test(Ex, exp(Text, ExpList)) :- !,
    read_term_from_atom(Text, P, [variable_names(Vars)]),
    testCall(Ex, P, Text), testExp(Ex, Text, Vars, ExpList).
test(_, ckA(_, [])) :- !.
test(Ex, ckA(Pred, [Test|Tests])) :- !,
    swritef(S, '%w(%w)', [Pred, Test]),
    read_term_from_atom(S, P, []),
    testCall(Ex, P, S), test(Ex, ckA(Pred, Tests)).
test(_, ech(_, [])) :- !.
test(Ex, ech(Text, [Cond|Conds])) :- !,
    swritef(S, '%w|%w', [Text, Cond]),
    read_term_from_atom(S, P|Q, [variable_names(Vars)]),
    forall(P, (
               swritef(Msg, '%w pentru soluția %w a predicatului %w', [Cond, Vars, Text]),
               testCall(Ex, Q, Msg))),
    test(Ex, ech(Text, Conds)).
test(Ex, nsl(Text, Tmplt, N)) :- !,
    swritef(S, 'findall(%w, %w, TheList)', [Tmplt, Text]),
    read_term_from_atom(S, P, [variable_names(Vars)]),
    testCall(Ex, P, S), testNSols(Ex, Text, Vars, N).
test(Ex, sls(Text, Tmplt, Sols)) :- !,
    swritef(S, 'findall(%w, %w, TheList)', [Tmplt, Text]),
    read_term_from_atom(S, P, [variable_names(Vars)]),
    testCall(Ex, P, S), testSols(Ex, Text, Vars, Sols).
test(Ex, sSO(Text, Tmplt, Sols)) :- !,
    swritef(S, 'setof(%w, %w, TheList)', [Tmplt, Text]),
    read_term_from_atom(S, P, [variable_names(Vars)]),
    testCall(Ex, P, S), testSols(Ex, Text, Vars, Sols).
test(Ex, _) :- failure(Ex, 'INTERN: Test necunoscut').

% Pentru exercițiul Ex, evaluează clauza Do, dată ca termen.
% Opțional, în mesajul de eroare interogarea poate fi afișată ca
% parametrul Text.
testCall(Ex, Do) :- swritef(Text, '%q', [Do]), testCall(Ex, Do, Text).
testCall(Ex, Do, Text) :-
        catch((call(Do), !
              ;   !, swritef(M, 'Interogarea %w a esuat.', [Text]), failure(Ex, M)
              ), Exc,
              (swritef(M, 'Interogarea %w a produs exceptie: %w', [Text, Exc]),
              except(Ex, M))
             ).

testExp(_, _, _, []) :- !.
testExp(Ex, Text, Vars, [v(Var) | Rest]) :- !,
    (   getVal(Var, Vars, V), !,
        (   var(V), !, testExp(Ex, Text, Vars, Rest) ;
            swritef(M, 'Interogarea %w leaga %w (la valoarea %w) dar nu ar fi trebuit legata.',
                    [Text, Var, V]), failure(Ex, M)
        )
    ;
    swritef(M, 'INTERN: Interogarea %w nu contine variabila %w.', [Text, Var]),
    failure(Ex, M)
    ).
testExp(Ex, Text, Vars, [set(Var, Set) | Rest]) :- !,
    (   getVal(Var, Vars, V), !,
        testSet(Ex, Text, 'intoarce', V, Set),
        testExp(Ex, Text, Vars, Rest)
    ;
    swritef(M, 'INTERN: Interogarea %w nu contine variabila %w.', [Text, Var]),
    failure(Ex, M)
    ).
testExp(Ex, Text, Vars, [setU(Var, Set) | Rest]) :- !,
    (   getVal(Var, Vars, V), !,
        testSetU(Ex, Text, 'intoarce', V, Set),
        testExp(Ex, Text, Vars, Rest)
    ;
    swritef(M, 'INTERN: Interogarea %w nu contine variabila %w.', [Text, Var]),
    failure(Ex, M)
    ).
testExp(Ex, Text, Vars, [cond(Cond) | Rest]) :- !,
    swritef(S, "(%w, %w)", [Text, Cond]),
    read_term_from_atom(S, P, []),
    (
        call(P), !, testExp(Ex, Text, Vars, Rest)
        ;
        swritef(M, 'Dupa interogarea %w conditia %w nu este adevarata.', [Text, Cond]),
        failure(Ex, M)
    ).
testExp(Ex, Text, Vars, [Var, Val | Rest]) :- !,
    (   getVal(Var, Vars, V), !,
        (   V == Val, !, testExp(Ex, Text, Vars, Rest) ;
            swritef(M, 'Interogarea %w leaga %w la %w in loc de %w.',
                    [Text, Var, V, Val]), failure(Ex, M)
        )
    ;
    swritef(M, 'INTERN: Interogarea %w nu contine variabila %w.', [Text, Var]),
    failure(Ex, M)
    ).
testExp(Ex, _, _, [X | _]) :- !,
        swritef(M, 'INTERN: element necunoscut pentru exp: %w', [X]),
        failure(Ex, M).
testExp(Ex, _, _, X) :- !,
        swritef(M, 'INTERN: format gresit pentru exp: %w', [X]),
        failure(Ex, M).

testNSols(Ex, Text, Vars, N) :-
    (   getVal('TheList', Vars, V), length(V, NSols), !,
        (   NSols =:= N, !
        ;   swritef(M, 'Numarul de solutii pentru %w este %w in loc de %w.',
                    [Text, NSols, N]), failure(Ex, M)
        )
    ;   failure(Ex, 'INTERNAL: nu avem variabila TheList sau aceasta nu este lista.')
    ).

testSols(Ex, Text, Vars, Sols) :-
    (   getVal('TheList', Vars, V), !,
        testSet(Ex, Text, 'are ca solutii', V, Sols)
    ;   failure(Ex, 'INTERNAL: nu avem variabila TheList sau aceasta nu este lista.')
    ).

testSetU(Ex, Text, TypeText, SetG, SetE) :- sort(SetG, SetGUnique),
    testSet(Ex, Text, TypeText, SetGUnique, SetE).
testSet(Ex, Text, TypeText, SetG, SetE) :-
    msort(SetG, SetGSorted), msort(SetE, SetESorted),
    (   SetGSorted == SetESorted, ! ;
        testSetMinus(SetG, SetE, TooMuch),
        testSetMinus(SetE, SetG, TooLittle),
        (   TooMuch == [], TooLittle == [], !,
            M1 = 'vezi duplicate'
        ;   swritef(M1, '%w sunt in plus, %w lipsesc', [TooMuch, TooLittle])
        ),
        swritef(M,
                'Interogarea %w %w %w dar se astepta %w (%w)',
                [Text, TypeText, SetG, SetE, M1]), failure(Ex, M)
    ).

testSetMinus(From, ToRemove, Result) :-
        findall(E, (member(E, From), \+ member(E, ToRemove)), Result).

getVal(Var, [Var=Val | _], Val) :- !.
getVal(Var, [_ | Vars], Val) :- getVal(Var, Vars, Val).
