﻿%% LABORATOR 09
%% Prolog - Intro


lungime([],0).
lungime([_|R], N):- lungime(R,N1), N is N1 + 1.

membru(Elem,[Elem|_]).
membru(Elem,[_|Rest]) :- membru(Elem,Rest).

remove(E,[E|R],R).
remove(E,[F|R],[F|L]):- remove(E,R,L).

perm([],[]).
perm([F|R],P):- perm(R,P1), remove(F,P,P1).


:- discontiguous exercitiul/2.
%% -----------------------------------------------------------------------------

exercitiul(1, [1, punct]).
%% myConcat/3
%% myConcat(?List1, ?List2, ?List)
%% 'List' este lista formată prin concatenarea listelor 'List1' și
%% 'List2' si functioneaza similar cu un parametru acumulator.

%% Hint: Predicatul myConcat este adevărat dacă primul element al
%% rezultatului (List) este egal cu primul element al listei List1, iar
%% restul lui List este egal cu concatenarea restului lui List1 cu
%% List2.

myConcat([],L,L).
myConcat([A|B],C,[A|D]) :- myConcat(B, C, D).

check1:-
    tests([
        exp('myConcat([], [], L)', ['L', []]),
        exp('myConcat(L, [1,2], [1,2])', ['L', []]),
        exp('myConcat([1,2,3], L, [1,2,3,4,5])', ['L', [4, 5]]),
        chk(myConcat([a, b, c], [d], [a, b, c, d])),
        chk(myConcat([X1, X2], [X1, X2], [1, 2, 1, 2])),
        uck(myConcat([X1, X2], [X1, X2], [1, 2, 3, 4])),
        nsl('myConcat([_,_,_], [_], L)', 'L', 1),
        nsl('myConcat([_,_,_], L, [_,_,_,_])', 'L', 1),
        exp('myConcat([X51],[X52],[X53,X54])', [cond('X51 == X53'), v('X51'), cond('X52 == X54'), v('X54')])]),
        writeln('Exercițiul 1 rezolvat corect!').



%% -----------------------------------------------------------------------------
exercitiul(2, [0.5, puncte]).
%% myReverse/2
%% myReverse(?List, +RevList)
%% 'RevList' este o listă ce conține elementele listei 'List' în ordine inversă.
%% Regulile pot conține și predicatul myConcat/3.

%% Hint: Predicatul este adevărat dacă RevList are ca ultim element
%% primul element din L, iar restul lui RevList este inversul restului
%% elementelor din L.

myReverse([],[]).
myReverse([H1|T1],L2):- myReverse(T1,H2), myConcat(H2,[H1],L2).

check2:-
    tests([
        chk(myReverse([], [])),
        chk(myReverse([1,2,3], [3,2,1])),
        exp('myReverse([1,2,3], Rev)', ['Rev', [3,2,1]]),
        exp('myReverse(List, [3,2,1])', ['List', [1,2,3]]),
        exp('myReverse([1,X2,X3], [3,2,X1])', ['X1', 1, 'X2', 2, 'X3', 3]),
        exp('myReverse([Y1,Y2], L)', [cond('L == [Y2, Y1]'), v('Y1'), v('Y2')]),
        exp('myReverse(L, [Z])', [cond('L == [Z]'), v('Z')]),
        nsl('myReverse([_,_], X)', 'X', 1)]),
        writeln('Exercițiul 2 rezolvat corect!').



%% -----------------------------------------------------------------------------
exercitiul(3, [0.5, puncte]).
%% myReverseAcc/3
%% myReverseAcc(?List, ?Acc, ?RevList)
%% 'RevList' este o listă ce conține elementele listei 'List' în ordine inversă
%% și elementele listei 'Acc'.
%% (Indicație: 'Acc' se va comporta precum un acumulator)
%% Obs. Regulile vor folosi doar predicatul myReverseAcc(și ",").

%% Hint: Inversul listei se va construi in Acc. Predicatul este adevărat
%% dacă valoarea primului element al acumulatorului, pentru restul lui
%% List, este egal cu primul element din List.


myReverseAcc([], L, L).
myReverseAcc([H|T],A,L):- myReverseAcc(T,[H|A],L).

check3:-
    tests([
        1, chk(myReverseAcc([], [], [])),
        1, chk(myReverseAcc([1,2,3], [0], [3,2,1,0])),
        2, exp('myReverseAcc([1,2,3], [0], Rev)', ['Rev', [3,2,1,0]]),
        2, exp('myReverseAcc(List, [0], [3,2,1,0])', ['List', [1,2,3]]),
        2, exp('myReverseAcc([X2,1], [3], [X1,2,3])', ['X1', 1, 'X2', 2])]),setDiff([], _, []).

%% -----------------------------------------------------------------------------

exercitiul(4, [0.5, puncte]).
%% numToBase/3
%% numToBase(+N, +B, -Nb)
%% Nb este reprezentarea lui N in baza B.
%% Vom considera B <= 10.

%% Hint: Predicatul este adevărat dacă Nb este egal cu Qb * 10 + Rest,
%% unde Qb reprezintă valoarea lui N // B în baza B, iar Rest este
%% restul împărtirii lui N la B.

numToBase(0, _, 0).
numToBase(N, B, Nb):- Quotient is N//B,
                      Rest is N mod B,
                      numToBase(Quotient, B, Qb),
                      Nb is Qb * 10 + Rest.


check4:-
    tests([
        1, chk(numToBase(0, 2, 0)),
        1, chk(numToBase(8, 2, 1000)),
        1, chk(numToBase(63, 5, 223)),
        1, chk(numToBase(76, 10, 76)),
        1, exp('numToBase(17, 3, X)', ['X', 122])]).


%% -----------------------------------------------------------------------------

exercitiul(5, [0.5, puncte]).
%% factorial/2
%% factorial(+N, -Fact)
%% 'Fact' este factorialul lui 'N'.
%% N va fi mereu legat la un număr natural.

%% Hint: Predicatul este adevărat dacă F este egal cu N*(N-1)!.

factorial(0, 1).
factorial(N, F):-
        N > 0,
        N1 is N - 1,
        factorial(N1, F1),
        F is N * F1.

check5:-
	tests([
        exp('factorial(1, F1)',['F1',1]),
	exp('factorial(4, F2)',['F2',24]),
        chk(factorial(5, 120)),
	chk(factorial(6, 720)),
	chk(factorial(7, 5040))]),
        writeln('Exercițiul 5 rezolvat corect!').


%% -----------------------------------------------------------------------------
exercitiul(6,[1, punct]).
%% palindrom/2
%% palindrom(+List)
%% 'List' este un palindrom.

%% Hint: Predicatul este adevărat dacă inversul lui List este List.

palindrom(L):-
        myReverseAcc(L,[],L).

check6 :-
	tests([
	chk(palindrom([1,2,3,2,1])),
	chk(palindrom([1,2,3,3,2,1])),
	uck(palindrom([1,2,3,0,2,1])),
	exp('palindrom([1,2,3,X3,X2,X1])', ['X1', 1, 'X2', 2, 'X3', 3]),
	uck(palindrom([1,2,3,X,_,X]))]),
        writeln('Exercițiul 6 rezolvat corect!').


%% -----------------------------------------------------------------------------

exercitiul(7,[1, punct]).
%% setIntersection/3
%% setIntersection(+L1, +L2, -L)
%% L este intersectia listelor L1 si L2.

%% Hint: Predicatul este adevărat dacă primul element din L3 este egal
%% cu primul element din L1 (H1), dacă H1 este si în lista L2.

setIntersection([], _, []).
setIntersection([H1|T1], L2, L3):-membru(H1, L2),
                               L3 = [H1|T3],
                               setIntersection(T1, L2, T3).
setIntersection([_|T1], L2, L3):-setIntersection(T1, L2, L3).

check7:-
    tests([
        1, chk(setIntersection([], [1,2], [])),
        1, chk(setIntersection([1,2,3], [7,9,24], [])),
        1, chk(setIntersection([1,2,3], [2], [2])),
        2, exp('setIntersection([1,2,3,4,7,13], [7,13,21], Int)', ['Int', [7,13]])]).

%% -----------------------------------------------------------------------------

exercitiul(8,[1, punct]).
%% setDiff/3
%% setDiff(+L1, +L2, -L)
%% L este diferenta listelor L1 si L2 (L1 - L2)

%% Hint: Predicatul este adevărat dacă primul element din L3 este egal
%% cu primul element din L1 (H1), dacă H1 nu este si în lista L2.

setDiff([], _, []).
setDiff([H1|T1], L2, [H1|T3]):-not(membru(H1, L2)),
                               setDiff(T1, L2, T3).
setDiff([H1|T1], L2, L3):-membru(H1, L2),
                          setDiff(T1, L2, L3).


check8:-
    tests([
        1, chk(setDiff([], [1,2], [])),
        1, chk(setDiff([1,2,3], [7,9,24], [1,2,3])),
        1, chk(setDiff([1,2,3], [2], [1,3])),
        2, exp('setDiff([1,2,3,4,7,13], [7,13,21], Diff)', ['Diff', [1,2,3,4]])]).


%% -----------------------------------------------------------------------------

exercitiul(9,[1, punct]).
%% setUnion/3
%% setUnion(+L1, +L2, -L)
%% L este reuniunea listelor L1 si L2.

%% Hint: Predicatul este adevărat dacă L este egal cu
%% L1 ++ (L2 - (L1 intersectat L2))



setUnion(L1, L2, L):- setIntersection(L1, L2, Int),
                      setDiff(L2, Int, Diff),
                      myConcat(L1, Diff, L).

check9:-
    tests([
        1, chk(setUnion([], [1,2], [1,2])),
        1, chk(setUnion([1,2,3], [7,9,24], [1,2,3,7,9,24])),
        1, chk(setUnion([1,2,3], [2], [1,2,3])),
        2, exp('setUnion([1,2,3,4,7,13], [29,3,7,13,21], Union)', ['Union', [1,2,3,4,7,13,29,21]])]).



%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
%% Se dau următoarele fapte ce descriu arcele unei păduri de arbori binari.
%% Fiecare nod poate avea maxim doi fii.

nod(a). nod(b). nod(c). nod(d). nod(e). nod(f). nod(g).
nod(h). nod(i). nod(j). nod(k). nod(l).
nod(m).
nod(n). nod(p). nod(o).

%% -----------------------------------------------------------------------------
%% arc/2

arc(a,b). arc(a,c). arc(b,d). arc(c,e). arc(c,g). arc(e,f).
arc(h,i). arc(h,j). arc(i,k). arc(j,l).
arc(n,o). arc(o,p).

%% -----------------------------------------------------------------------------
exercitiul(10, [0.5, puncte]).
%% isLeaf/1
%% isLeaf(?Nod)

%% Hint: Predicatul este adevărat dacă Nod este nod si nu există arcuri
%% care pornesc din Nod.

isLeaf(X):- nod(X), \+ arc(X,_).

check10:-
	tests([
	chk(isLeaf(d)),
	chk(isLeaf(f)),
	chk(isLeaf(g)),
	chk(isLeaf(k)),
	chk(isLeaf(l)),
	chk(isLeaf(m)),
	chk(isLeaf(p)),
	uck(isLeaf(a)),
	uck(isLeaf(b)),
	uck(isLeaf(c)),
	uck(isLeaf(e)),
	nsl('isLeaf(A)', 'A', 7)]),
        writeln('Exercițiul 10 rezolvat corect!').



%% -----------------------------------------------------------------------------
exercitiul(11, [0.5, puncte]).
%% isRoot/1
%% isRoot(?Nod)

%% Hint: Predicatul este adevărat dacă Nod este nod si nu există arcuri
%% care au ca destinatie Nod.

isRoot(X):- nod(X), \+ arc(_,X).

check11:-
	tests([
	chk(isRoot(a)),
	chk(isRoot(h)),
	chk(isRoot(m)),
	chk(isRoot(n)),
	uck(isRoot(b)),
	uck(isRoot(d)),
	uck(isRoot(e)),
	uck(isRoot(l)),
	nsl('isRoot(A)', 'A', 4)]),
        writeln('Exercițiul 11 rezolvat corect!').



%% -----------------------------------------------------------------------------
exercitiul(12, [1, punct]).
%% descendantOf/2
%% descendantOf(?X,?Y)
%% Nodul X este un urmaș a lui Y.

%% Hint: Predicatul este adevărat dacă există un arc de la Y la X sau
%% dacă există arc de la unul din urmasii lui Y la X.


descendantOf(X,Y):-arc(Y,X).
descendantOf(X,Y):-arc(Z,X), descendantOf(Z,Y).

check12:-
	tests([
	chk(descendantOf(b, a)),
	chk(descendantOf(c, a)),
	chk(descendantOf(d, b)),
	chk(descendantOf(j, h)),
	chk(descendantOf(l, j)),
	chk(descendantOf(j, h)),
	chk(descendantOf(f, a)),
	uck(descendantOf(a, _)),
	uck(descendantOf(h, _)),
	uck(descendantOf(m, _)),
	nsl('descendantOf(X, a)', 'X', 6),
	nsl('descendantOf(X, h)', 'X', 4),
	nsl('descendantOf(l, X)', 'X', 2),
	nsl('descendantOf(f, X)', 'X', 3)]),
        writeln('Exercițiul 12 rezolvat corect!').



%% -----------------------------------------------------------------------------
exercitiul(13, [2, puncte]).
%% descendants/2
%% descendants(?Nod, ?N)
%% Nodul Nod are N urmași.

%% Hint: Un nod frunză are 0 urmasi. Un nod X poate avea unul sau doi
%% urmasi directi. Numărul urmasilor unui nod este egal cu numărul
%% urmasilor directi + numărul urmasilor urmasilor directi.

descendants(X, 0):-
        isLeaf(X).
descendants(X, N):-
        arc(X, Y),
        \+ (arc(X, Z), Z \= Y),
        descendants(Y, N1),
        N is 1 + N1.
descendants(X, N):-
        arc(X, Y),
        arc(X, Z),
        Y @< Z,
        descendants(Y, N1),
        descendants(Z, N2),
        N is N1 + N2 + 2.

check13:-
	tests([
	chk(descendants(a, 6)),
	chk(descendants(b, 1)),
	exp('descendants(a, N)',['N', 6]),
	chk(descendants(c, 3)),
	chk(descendants(h, 4)),
	chk(descendants(g, 0)),
	chk(descendants(l, 0)),
	sls('descendants(X, 1)', 'X', [b, e, i, j, o]),
	sls('descendants(X, 2)', 'X', [n])]),
        writeln('Exercițiul 13 rezolvat corect!').




%%================
%%=====BONUS======
%%================
%% -----------------------------------------------------------------------------
exercitiul(14, [1, punct, bonus]).
%% sameTree/2
%% sameTree(+Nod, +Nod).

%% Hint: Predicatul este adevărat dacă Nod1 este urmasul lui Nod2, sau
%% Nod2 este urmasul lui Nod1, sau Nod1 si Nod2 sunt urmasi ai aceleiasi
%% rădăcini.

sameTree(A, B):- descendantOf(A, B).
sameTree(A, B):- descendantOf(B, A).
sameTree(A, B):- isRoot(C), descendantOf(A, C), descendantOf(B, C).


check14:-
	tests([
	chk(sameTree(a, b)),
	chk(sameTree(c, a)),
	chk(sameTree(c, e)),
	chk(sameTree(c, d)),
	chk(sameTree(n, o)),
	uck(sameTree(a, m)),
	uck(sameTree(c, n)),
	uck(sameTree(d, i))]),
        writeln('Exercițiul 14 rezolvat corect!').



%% -----------------------------------------------------------------------------
exercitiul(15, [2, puncte, bonus]).
%% drum/3
%% drum(?Nod, ?Nod, ?Lista)

drum(A, A, [A]).
drum(A, B, [A, B]):- \+ (\+ arc(A, B), \+ arc(B, A)).
drum(A, B, [A|T]):- arc(C, A), drum(C, B, T).
drum(A, B, [A|T]):- descendantOf(B, A), arc(A, C), descendantOf(B, C),drum(C, B, T).


check15:-
	tests([
	chk(drum(a, b, [a,b])),
	chk(drum(e, a, [e, c, a])),
	chk(drum(d, c, [d, b, a, c])),
	exp('drum(b, e, X)', ['X', [b, a, c, e]]),
	exp('drum(b, X, [b, a, c])', ['X', c]),
	exp('drum(X, b, [a, b])', ['X', a]),
	uck(drum(a, m, X)),
	uck(drum(a, X, [a, b, k]))
	]),
	writeln('Exercițiul 15 rezolvat corect!').



%% -----------------------------------------------------------------------------
exercitiul(16, [2, puncte, bonus]).
%% cost/3
%% cost(+Nod, +Nod, -Cost).
%% un arc in sus costa -1, unul in jos, 1.

cost(A, A, 0).
cost(A, B, 1):- arc(A, B), !.
cost(A, B, -1):- arc(B, A), !.
cost(A, B, N):- descendantOf(B, A), arc(A, C), descendantOf(B, C), cost(C, B, N2), !, N is N2 + 1.
cost(A, B, N):- arc(C, A), cost(C, B, N2), !, N is N2 - 1.

check16:-
	tests([
	chk(cost(a, a, 0)),
	chk(cost(a, b, 1)),
	chk(cost(c, a, -1)),
	chk(cost(a, d, 2)),
	uck(cost(a, h, X)),
	uck(cost(b,m,X))
	]),
        writeln('Exercițiul 16 rezolvat corect!').








%% ----------------------------------------
%% ----------------------------------------

%test_mode(vmchecker). % not implemented yet
test_mode(quick).



:-dynamic(punct/2).
:-dynamic(current/1).
%:-clean.

clean :- retractall(punct(_, _)), retractall(current(_)).

testtest(5).
testtest(a, 5).
testtest(b, _).
testtest(d, [2, 1, 3]).
testtest(e, [2, 1, 3, 1, 2]).
testtest(1, 2, 3).
testtest(c, X, X).

testtest :- tests([
               % chk(P) ("check") verifică evaluarea cu succes a lui P
               chk(testtest(5)), % a
               chk(testtest(6)), % b % eșuează
               % uck(P) ("uncheck") verifică evaluarea cu eșec a lui P
               uck(testtest(6)), % c
               uck(testtest(5)), % d % eșuează
               % exp(P, Exps) ("expect") verifică evaluarea cu succes a lui P și a unor condiții
               % P este dat ca șir de caractere pentru o verificare și afișare mai bune.
               % Condițiile sunt evaluate pe rând și independent unele de altele.
               % Dacă în lista de condiții avem un nume de variabilă,
               % se verifică că aceasta a fost legat la valoarea care urmează imediat în listă.
               % valoarea de verificat nu poate conține variabile din interogare
               % (pentru asta folosiți cond, vezi mai jos).
               exp('testtest(X, X)', ['X', b]), % e
               exp('testtest(X, Y, Z)', ['X', 1, 'Y', 2, 'Z', 3]), % f
               exp('testtest(X, X)', ['X', a]), % g % eșuează
               % Dacă în lista de condiții avem v('Var'), se verifică că Var a rămas nelegată.
               exp('testtest(b, Y)', [v('Y')]), % h
               exp('testtest(a, Y)', [v('Y')]), % i % eșuează
               % Dacă în lista de condiții avem cond('P'), se verifică că P este adevărat.
               % Diversele condiții din structuri cond diferite se verifică independent.
               exp('testtest(c, X, X)', [v('X'), cond('X==X')]), % j
               % Dacă în lista de condiții avem set('V', Set), se verifică că V este legată la mulțimea Set.
               % Duplicatele contează.
               exp('testtest(d, L)', [set('L', [1, 2, 3]), 'L', [1, 2, 3]]), % k % eșuează pe a 2a condiție
               exp('testtest(d, L)', [set('L', [2, 3, 4, 5])]), % l
               exp('testtest(e, L)', [set('L', [1, 2, 3])]), % m
               % setU funcționează la fel, dar ignoră duplicatele.
               exp('testtest(e, L)', [setU('L', [1, 2, 3])]), % n
               % nsl(P, Template, NSols) ("N Solutions") verifică dacă lungimea lui L din findall(P, Template, L) este NSols
               2, nsl('testtest(X, Y)', '(X, Y)', 4), % o
               % testul de mai sus valorează de 2 ori mai mult decât celelalte

               % sls(P, Template, Sols) ("Solutions") verifică dacă findall(P, Template, L) leagă L la aceeași mulțime cu Sols.
               % Duplicatele contează
               2, sls('testtest(X, X)', '(X, X)', [(b, b)]), % p
               % testul de mai sus valorează de 2 ori mai mult decât celelalte

               sls('testtest(X, Y)', '(X, Y)', [(b, 5)]) % q
           ]).


tests(Tests) :- (   current(_), ! ; retractall(punct(_, _))),
    total_fractions(Tests, TF),
    (   current(Ex), !, exercitiul(Ex, [Pts | _]), Total is TF / Pts
    ;   Total is TF / 100
    ),
    tests(Tests, 1, Total),
    (   current(_), !
    ;   findall(P, punct(_, P), L), sum_list(L, S), format('Rezolvat ~0f%.~n', [S])
    ).

tests([], _, _) :- !.
tests([P, T|R], Idx, Tot) :- number(P), !, test(T, Idx, P, Tot), tests(R, Idx+1, Tot).
tests([T|R], Idx, Tot) :- test(T, Idx, 1, Tot), tests(R, Idx+1, Tot).

total_fractions([], 0).
total_fractions([P, _|R], Tot) :- number(P), !, total_fractions(R, TotR), Tot is TotR + P.
total_fractions([_|R], Tot) :- total_fractions(R, TotR), Tot is TotR + 1.

test(T, Idx, Frac, Tot) :- IdxI is Idx + 96, char_code(CEx, IdxI),
    (   current(NEx), !, swritef(Ex, '[%w|%w]', [NEx, CEx]) ; swritef(Ex, '%w|', [CEx])),
    (once(test(Ex, T)), !, success(Ex, Frac, Tot) ; true).

success(Ex, Frac, Tot) :-
    Score is Frac / Tot,
    assert(punct(Ex, Score)),
    format('~w[OK] Corect. +~2f.~n', [Ex, Score]).

failure(Ex, M) :- format('~w[--] ~w~n', [Ex, M]), fail.

test(Ex, chk(P)) :- !, testCall(Ex, P).
test(Ex, uck(P)) :- !, testCall(Ex, \+ P).
test(Ex, exp(Text, ExpList)) :- !,
    read_term_from_atom(Text, P, [variable_names(Vars)]),
    testCall(Ex, P, Text), testExp(Ex, Text, Vars, ExpList).
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
test(Ex, _) :- failure(Ex, 'INTERNAL: Test necunoscut').

testCall(Ex, Do) :- swritef(Text, '%q', [Do]), testCall(Ex, Do, Text).
testCall(_, Do, _) :- call(Do), !.
testCall(Ex, _, Text) :- swritef(M, 'Interogarea %w a esuat.', [Text]), failure(Ex, M).

testExp(_, _, _, []) :- !.
testExp(Ex, Text, Vars, [v(Var) | Rest]) :- !,
    (   getVal(Var, Vars, V), !,
        (   var(V), !, testExp(Ex, Text, Vars, Rest) ;
            swritef(M, 'Interogarea %w leaga %w (la valoarea %w) dar nu ar fi trebuit legata.',
                    [Text, Var, V]), failure(Ex, M)
        )
    ;
    swritef(M, 'INTERNAL: Interogarea %w nu contine variabila %w.', [Text, Var]),
    failure(Ex, M)
    ).
testExp(Ex, Text, Vars, [set(Var, Set) | Rest]) :- !,
    (   getVal(Var, Vars, V), !,
        testSet(Ex, Text, 'intoarce', V, Set),
        testExp(Ex, Text, Vars, Rest)
    ;
    swritef(M, 'INTERNAL: Interogarea %w nu contine variabila %w.', [Text, Var]),
    failure(Ex, M)
    ).
testExp(Ex, Text, Vars, [setU(Var, Set) | Rest]) :- !,
    (   getVal(Var, Vars, V), !,
        testSetU(Ex, Text, 'intoarce', V, Set),
        testExp(Ex, Text, Vars, Rest)
    ;
    swritef(M, 'INTERNAL: Interogarea %w nu contine variabila %w.', [Text, Var]),
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
    swritef(M, 'INTERNAL: Interogarea %w nu contine variabila %w.', [Text, Var]),
    failure(Ex, M)
    ).

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
testSet(Ex, Text, TypeText, SetG, SetE) :- msort(SetG, SetGSorted),
    (   SetGSorted == SetE, ! ;
        setMinus(SetG, SetE, TooMuch),
        setMinus(SetE, SetG, TooLittle),
        (   TooMuch == [], TooLittle == [], !,
            M1 = 'vezi duplicate'
        ;   swritef(M1, '%w sunt in plus, %w lipsesc', [TooMuch, TooLittle])
        ),
        swritef(M,
                'Interogarea %w %w %w dar se astepta %w (%w)',
                [Text, TypeText, SetG, SetE, M1]), failure(Ex, M)
    ).

setMinus(From, ToRemove, Result) :-
        findall(E, (member(E, From), \+ member(E, ToRemove)), Result).

getVal(Var, [Var=Val | _], Val) :- !.
getVal(Var, [_ | Vars], Val) :- getVal(Var, Vars, Val).

check:-
    clean,
    forall(exercitiul(Ex, _),
           (   atom_concat(check, Ex, Ck),
               retractall(current(_)), assert(current(Ex)),
               once(call(Ck)) ; true)),
    findall(P, punct(_, P), L),
    sum_list(L, S),
    format('Punctaj total: ~f~n',[S]),
    clean.
