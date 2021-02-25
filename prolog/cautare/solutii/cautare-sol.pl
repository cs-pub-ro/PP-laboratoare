%% -------------------------------------------------------------
%% -------------------------------------------------------------

%% -- BACKTRACKING ATUNCI CÂND CUNOAȘTEM LUNGIMEA SOLUȚIEI --

%%% 1. (2p)
%% Înțelegeți predicatele solve_queens/1, template/1 și correct/1.
%% Observați că lipsește definiția predicatului safe/2.

%% template/1
%% template(?List)
%% List are forma unei soluții pentru problema celor opt regine.
%% Lungimea soluției este cunoscută și fixă.
template([1/_, 2/_, 3/_, 4/_, 5/_, 6/_, 7/_, 8/_]).

%% correct/1
%% correct(?Solution)
%% Solution reprezintă o soluție validă pentru problema celor opt regine.
correct([]):-!.
correct([X/Y|Others]):-
        correct(Others),
        member(Y, [1, 2, 3, 4, 5, 6, 7, 8]),
        safe(X/Y, Others).

%% solve_queens/1
%% solve_queens(-Solution)
%% Solution este o soluție a problemei celor opt regine.
solve_queens(S):-template(S), correct(S).

%% Scrieți predicatul safe/2 utilizat în rezolvarea problemei celor opt regine.
%% Predicatul va avea antetul safe(+X/Y, +Others) cu semnificația că se verifică
%% dacă plasarea reginei de coloana X pe linia Y nu se atacă cu o altă regină
%% din lista Others. Aceasta are forma [X1/Y1, X2/ Y2/ ...].

%% safe/2
%% safe(+X/Y, +Others)

safe(_, []):-!.
safe(X1/Y1, [X2/Y2 | Others]):-
        % o regină este safe față de o alta dacă
        X1 =\= X2, % nu sunt pe aceeași coloană
        Y1 =\= Y2, % nu sunt pe aceeași linie
        X1 - Y1 =\= X2 - Y2, % nu sunt pe aceeași diagonală
        X1 + Y1 =\= X2 + Y2,
        safe(X1/Y1, Others).

check1:-
    tests(1, [
        \+ safe(7/1, [8/1]),
        \+ safe(7/1, [8/2]),
        safe(7/1, [8/3]),
        \+ safe(7/5, [8/4]),
        safe(7/3, [8/5]),
        safe(4/1, [5/3, 6/5, 7/2, 8/4]),
        \+ safe(4/1, [5/4, 6/2, 7/7, 8/5]),
        safe(1/4, [2/6, 3/8, 4/2, 5/7, 6/1, 7/3, 8/5]),
        (   findall(Sol, solve_queens(Sol), All), length(All, L), L == 92)
          ]).

%% Întrebați-l pe Prolog "solve_queens(Sol)" pentru a vizualiza soluțiile.

%% -------------------------------------------------------------
%% -------------------------------------------------------------

%% -- BACKTRACKING ATUNCI CÂND NU CUNOAȘTEM LUNGIMEA SOLUȚIEI --

%%% 2. (6p)
%% Înțelegeți cum funcționeză predicatele solve și search pentru rezolvarea
%% unei probleme de căutare în spațiul stărilor. Observați utilizarea
%% predicatelor initial_state/1, final_state/1 și next_state/2.

search([CurrentState|Other], Solution):-
        final_state(CurrentState),
        !,
        reverse([CurrentState|Other], Solution).

search([CurrentState|Other], Solution):-
        next_state(CurrentState, NextState),
        \+ member(NextState, Other),
        search([NextState,CurrentState|Other], Solution).

solve(Solution):-
        initial_state(State),
        search([State], Solution).

%% Exemplu: problema țăranului, a lupului, a caprei și a verzei.
%% Vom reprezenta o stare astfel:
%% state(MalBarcă, MalȚăran, MalLup, MalCapră, MalVarză)

opus(est, vest).
opus(vest, est).

%%initial_state(state(est, est, est, est, est)).

%%final_state(state(_, vest, vest, vest, vest)).


%% Taranul calatoreste singur
%%next_state(state(MalBarca1, MalBarca1, MalLup, MalCapra, MalVarza),
%%           state(MalBarca2, MalBarca2, MalLup, MalCapra, MalVarza)):-
%%        opus(MalBarca1, MalBarca2),
%%        opus(MalLup, MalCapra),
%%        opus(MalCapra, MalVarza).

%% Taranul calatoreste cu lupul
%%next_state(state(MalBarca1, MalBarca1, MalBarca1, MalCapra, MalVarza),
%%           state(MalBarca2, MalBarca2, MalBarca2, MalCapra, MalVarza)):-
%%        opus(MalBarca1, MalBarca2),
%%        opus(MalCapra, MalVarza).

%% Taranul calatoreste cu capra
%%next_state(state(MalBarca1, MalBarca1, MalLup, MalBarca1, MalVarza),
%%           state(MalBarca2, MalBarca2, MalLup, MalBarca2, MalVarza)):-
%%        opus(MalBarca1, MalBarca2).

%% Taranul calatoreste cu varza
%%next_state(state(MalBarca1, MalBarca1, MalLup, MalCapra, MalBarca1),
%%           state(MalBarca2, MalBarca2, MalLup, MalCapra, MalBarca2)):-
%%        opus(MalBarca1, MalBarca2),
%%        opus(MalLup, MalCapra).



%% Rescrieți predicatele initial_state/1, final_state/1, și next_state/2 pentru
%% a rezolva problema soților geloși.

%% Pentru o mai bună structură, implementați întâi predicatele boat/1
%% și safe/1 detaliate mai jos.

%% Problema Soților Geloși
%% Fie un râu cu două maluri, trei cupluri și o barcă. Barca și cele trei
%% cupluri se află inițial pe un mal, iar scopul este ca toți să ajungă pe
%% malul opus. Barca are capacitate de maximum două persoane și nu poate
%% călători fără nicio persoană. Găsiți o secvență de traversări, astfel
%% încât nicio femeie să nu poată fi în prezența altor bărbați, decât dacă
%% soțul acesteia este prezent.

%% Predicate utile: sort/2, @</2 (vedeți help)

%% Atenție, nu lăsați în spațiul de lucru predicatele pentru ambele
%% probleme!

% married(?Woman, ?Man)
married(1, a).
married(2, b).
married(3, c).
% woman(?Woman)
woman(W) :- married(W, _).
% man(?Man)
man(M) :- married(_, M).

% people(-AllPeople)
% Obține lista cu toate persoanele.
allPeople(AllPeople) :- findall(Person, (married(Person, _) ; married(_, Person)), AllPeople).

% setMinus(+From, +ToRemove, -Result)
% calculează diferența de mulțimi From \ ToRemove
setMinus(From, ToRemove, Result) :-
        findall(E, (member(E, From), \+ member(E, ToRemove)), Result).

% setPlus(+A, +B, -Result
% concatenează A și B în Result (Atenție! nu elimină duplicate)
setPlus(A, B, Result) :- append(A, B, Result).

% subSet(+Smaller, +Bigger)
% Verifică dacă setul Smaller este inclus în sau egal cu setul Bigger
subSet([], []).
subSet(Smaller, [_|Bigger]) :- subSet(Smaller, Bigger).
subSet([E|Smaller], [E|Bigger]) :- subSet(Smaller, Bigger).

% boat/1
% boat(?People)
% o listă de 1 sau două persoane care pot merge împreună în barcă.
% boat(_) :- fail.

% în barcă se pot afla
boat([F]) :- woman(F). % o femeie
boat([M]) :- man(M). % un bărbat
boat([F, M]) :- married(F, M). % un cuplu
boat([F1, F2]) :- woman(F1), woman(F2), F1 @< F2. % 2 femei diferite, sortate
boat([M1, M2]) :- man(M1), man(M2), M1 @< M2. % 2 bărbați diferiți, sortați

% safe/1
% safe(+PeopleList)
% verifică dacă persoanele din listă pot sta pe același mal.
% safe(_) :- fail.

% un grup de oameni este valid în raport cu problema dacă
% nu există niciun bărbat în grup
safe(People) :- \+ (man(MX), member(MX, People)), !.
% sau, dacă există un bărbat,
safe(People) :- man(MX), member(MX, People), !,
        % toate femeile din grup au soțul în acest grup
        forall((woman(W), member(W, People), married(W, M)),
                        member(M, People)).

initial_state(state(est, P)) :- allPeople(P).
final_state(state(vest, P)) :- allPeople(P).

% reprezentare a stării ca maulul pe care este barca și persoanele care
% sunt pe malul cu barca.
%
% La o trecere avem așa:
% People1 sunt persoanele de pe malul de unde a plecat barca (malul 1),
% înainte ca aceasta să plece; People2 sunt persoanele de pe malul unde
% ajunge barca (malul 2), după ce aceasta a ajuns.
next_state(state(Mal1, People1), state(Mal2, People2)) :-
        % barca trece de pe un mal pe altul
        opus(Mal1, Mal2),
        allPeople(All),
        % în barcă trec un grup valid de persoane
        boat(Boat),
        % persoanele care trec erau toate printre cei de pe malul inițial (1)
        subSet(Boat, People1),
        % People12 sunt persoanele care rămân pe malul 1
        setMinus(People1, Boat, People12),
        % People2 sunt persoanele de pe malul 2 înainte de ajungerea bărcii
        setMinus(All, People1, People2),
        % People22 sunt oamenii de pe malul 2, după ce ajunge barca
        setPlus(People2, Boat, People22),
        % verificăm că după ce trece barca grupurile de persoane sunt 'safe'
        safe(People12), safe(People22),
        % sortăm People22 pentru a evita mai multe stări identice cu reprezentare diferită
        sort(People22, People2).
        %format("~w -~w-> ~w~n", [state(Mal1, People1), Boat, state(Mal2, People2)]).


check2L :- tests(2/a, [
                  boat([1]),
                  boat([b]),
                  boat([1, 2]),
                  boat([b, c]),
                  boat([1, a]),
                  boat([3, c]),
                  \+ boat([1, b]),
                  \+ boat([3, a]),
                  (   findall(B, boat(B), L), length(L, LL), LL == 15 ),
                  safe([2, 3, a, b, c]),
                  safe([1, 2, 3]),
                  safe([1]),
                  safe([1, 2, a, b]),
                  \+ safe([1, 2, b, c]),
                  \+ safe([2, c]),
                  (   allPeople(P), setof(S, (subSet(S, P), safe(S)), L2),
                      length(L2, LL2), LL2 == 34 ),
                  (   allPeople(P), setof(S, (subSet(S, P), \+ safe(S)), L3),
                      length(L3, LL3), LL3 == 30 ),
                  (   allPeople(P), findall(S, (subSet(S, P), safe(S)), L4),
                      length(L4, LL4), LL4 == 34 ),
                  (   solve(X), length(X, LSol), LSol == 12)
                 ]).
check2 :- check2L,
        tests(2/b, [(   findall(Sol, (solve(Sol), length(Sol, 12)), MinSols),
                      length(MinSols, Len), Len == 486)
                  ]).

%% -------------------------------------------------------------
%% -------------------------------------------------------------

%%% 3 (4p)
%% 3a (2p)
%% Implementați un predicat bfs/3 care să descrie un mecanism de căutare în
%% lățime într-un graf. Se dau predicatele initial_node/1, final_node/1 și
%% edge/2. Observați similaritatea cu initial_state/1, final_state/1 și
%% next_state/2.

do_bfs(Solution):-
        initial_node(StartNode),
        bfs([(StartNode,nil)], [], Discovered),
        extract_path(Discovered, Solution).

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


%% 3b (BONUS - 2p)
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

%%% Testare

edge(a,b). edge(a,c). edge(a,d).
edge(c,e). edge(c,f).
edge(d,h).
edge(e,a). edge(e,g).
edge(f,a). edge(f,g).
edge(g,h).

initial_node(a).
final_node(h).

check3a:-
		bfs([(a,nil)], [], R),
        R == [(h, d), (f, c), (e, c), (d, a), (c, a), (b, a), (a, nil)],
        writeln('.Exercițiul 3a rezolvat corect!'),
        !.
check3b:-
		extract_path([(h, d), (f, c), (e, c), (d, a), (c, a), (b, a), (a, nil)], R),
        R == [a, d, h],
        writeln('.Exercițiul 3b (BONUS) rezolvat corect!'),
        !.

%% -------------------------------------------------------------
%% -------------------------------------------------------------

%%% 4. BONUS. (3p)
%% BFS poate fi folosit pentru a testa dacă un graf este bipartit. Căutarea
%% începe pe rând din fiecare nod și etichetează alternativ cu 0 sau 1 fiecare
%% nod descoperit. De exemplu, nodul de start are eticheta 0, toți vecinii săi
%% primesc eticheta 1, vecinii acestora primesc iarăși 0 etc. Dacă la un moment
%% dat un nod are vecini cu aceeași etichetă ca a lui, atunci graful nu e bipartit.
%% Scrieți un predicat bipartite care testează dacă graful dat prin fapte de tip
%% nod(Nod) și arc(Sursă, Destinație) este bipartit.

%% bipartite/0

not_bipartite([(Node, Partition)|Rest], Visited):-
	findall(Kid, (arc(Node, Kid), (member((Kid, Partition), Rest) ; member((Kid, Partition), Visited))), [_|_]), !.
not_bipartite([(Node, Partition)|Rest], Visited):-
	OtherPartition is 1 - Partition,
	findall((Kid, OtherPartition), (arc(Node, Kid), \+member((Kid, _), Rest), \+member((Kid, _), Visited)), Next),
	append(Rest, Next, Queue),
	not_bipartite(Queue, [(Node, Partition)|Visited]).

bipartite:-
	\+ (nod(Node), not_bipartite([(Node, 0)], [])).

%%% Testare
nod(a). nod(b). nod(c). nod(d). nod(e). nod(f). nod(g). nod(h).
arc(a,b). arc(b,a). arc(a,d). arc(d,a). arc(a,e). arc(e,a).
arc(b,c). arc(c,b). arc(b,f). arc(f,b).
arc(c,d). arc(d,c). arc(c,g). arc(g,c).
arc(d,h). arc(h,d). % arc(f, a).
arc(e,f). arc(f,e). arc(e,h). arc(h,e).
arc(f,g). arc(g,f).
arc(g,h). arc(h,g).

check4:-
        bipartite,
        writeln('.Exercițiul BONUS rezolvat corect!'),
        !.
%% -------------------------------------------------------------
%% -------------------------------------------------------------

:- dynamic punct/2.

tests(Ex, [], _) :- !, format('Exercitiul ~w a fost rezolvat corect.~n', [Ex]).
tests(Ex, [T | R], Idx) :-
        Idx1 is Idx + 1,
        (   call(T), !, write('.'), tests(Ex, R, Idx1);
        format('Esec la exercitiul ~w testul ~w: ~n    ~w~n',
               [Ex, Idx1, T]),
            !, fail).
tests(Ex, L) :- tests(Ex, L, 0).

check:-
        retractall(punct(_, _)),
        once((check1, assert(punct(1, 2)) ; assert(punct(1, 0)))),
        once((check2, assert(punct(2, 6)) ; assert(punct(2, 0)))),
        once((check3a, assert(punct(31, 2)) ; assert(punct(31, 0)))),
        once((check3b, assert(punct(32, 2)) ; assert(punct(32, 0)))),
        once((check4, assert(punct(4, 3)) ; assert(punct(4, 0)))),
        fail.

check:-
        findall(P, punct(_, P), L),
        sum_list(L, S),
        format('Punctaj total: ~f~n',[S]).

