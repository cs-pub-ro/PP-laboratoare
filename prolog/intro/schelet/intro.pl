%% Prolog - Intro
:- discontiguous exercitiul/2.
:- ensure_loaded('checker.pl').
%% -----------------------------------------------------------------------------

%% Pentru a testa laboratorul, folositi check. Pentru a testa exercitiul
%% N, folositi checkN.

exercitiul(1, []).
%% myConcat/3
%% myConcat(?List1, ?List2, ?List)
%% 'List' este lista formată prin concatenarea listelor 'List1' și
%% 'List2'.

%% Hint: Predicatul myConcat este adevărat dacă primul element al
%% rezultatului (List) este egal cu primul element al listei List1, iar
%% restul lui List este egal cu concatenarea restului lui List1 cu
%% List2.


myConcat(_,_,_):- false.

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
        exp('myConcat([X51],[X52],[X53,X54])', [cond('X51 == X53'), v('X51'), cond('X52 == X54'), v('X54')])]).



%% -----------------------------------------------------------------------------
exercitiul(2, []).
%% myReverse/2
%% myReverse(?List, ?RevList)
%% 'RevList' este o listă ce conține elementele listei 'List' în ordine inversă.
%% Regulile pot conține și predicatul myConcat/3.

%% Hint: Predicatul este adevărat dacă RevList are ca ultim element
%% primul element din L, iar prima parte a lui RevList (de la primul
%% element pâna la penultimul) este inversul restului elementelor din L.


myReverse(_,_):- false.

check2:-
    tests([
        chk(myReverse([], [])),
        chk(myReverse([1,2,3], [3,2,1])),
        exp('myReverse([1,2,3], Rev)', ['Rev', [3,2,1]]),
        exp('myReverse(List, [3,2,1])', ['List', [1,2,3]]),
        exp('myReverse([1,X2,X3], [3,2,X1])', ['X1', 1, 'X2', 2, 'X3', 3]),
        exp('myReverse([Y1,Y2], L)', [cond('L == [Y2, Y1]'), v('Y1'), v('Y2')]),
        exp('myReverse(L, [Z])', [cond('L == [Z]'), v('Z')]),
        nsl('myReverse([_,_], X)', 'X', 1)]).




%% -----------------------------------------------------------------------------
exercitiul(3, []).
%% myReverseAcc/3
%% myReverseAcc(?List, ?Acc, ?RevList)
%% 'RevList' este o listă ce conține elementele listei 'List' în ordine inversă
%% și elementele listei 'Acc'.
%% (Indicație: 'Acc' se va comporta precum un acumulator)
%% Obs. Regulile vor folosi doar predicatul myReverseAcc(și ",").

%% Hint: Inversul listei se va construi în Acc. Predicatul este adevărat
%% dacă în apelul pentru restul lui List, valoarea primului element al
%% acumulatorului este egală cu primul element din List, iar al treilea
%% argument este același pentru apelul curent și pentru apelul recursiv
%% (rezultatul se întoarce neschimbat de la sfârșitul recursivității
%% până la apelul inițial).


myReverseAcc(_,_,_):- false.

check3:-
    tests([
        1, chk(myReverseAcc([], [], [])),
        1, chk(myReverseAcc([1,2,3], [0], [3,2,1,0])),
        2, exp('myReverseAcc([1,2,3], [0], Rev)', ['Rev', [3,2,1,0]]),
        2, exp('myReverseAcc(List, [0], [3,2,1,0])', ['List', [1,2,3]]),
        2, exp('myReverseAcc([X2,1], [3], [X1,2,3])', ['X1', 1, 'X2', 2])]).


%% -----------------------------------------------------------------------------
exercitiul(4, []).
%% factorial/2
%% factorial(+N, -Fact)
%% 'Fact' este factorialul lui 'N'.
%% N va fi mereu legat la un număr natural.

%% Hint: Predicatul este adevărat dacă F este egal cu N*(N-1)!.


factorial(_, _):- false.

check4:-
	tests([
        exp('factorial(1, F1)',['F1',1]),
	exp('factorial(4, F2)',['F2',24]),
        chk(factorial(5, 120)),
	chk(factorial(6, 720)),
	chk(factorial(7, 5040))]).


%% -----------------------------------------------------------------------------
exercitiul(5, []).
%% palindrom/2
%% palindrom(+List)
%% 'List' este un palindrom.

%% Hint: Predicatul este adevărat dacă inversul lui List este List.


palindrom(_):- false.

check5 :-
	tests([
	chk(palindrom([1,2,3,2,1])),
	chk(palindrom([1,2,3,3,2,1])),
	uck(palindrom([1,2,3,0,2,1])),
	exp('palindrom([1,2,3,X3,X2,X1])', ['X1', 1, 'X2', 2, 'X3', 3]),
	uck(palindrom([1,2,3,X,_,X]))]).


%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
%% Se dau următoarele fapte ce descriu arcele unei păduri de arbori binari.

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
exercitiul(6, []).
%% isLeaf/1
%% isLeaf(?Nod)

%% Hint: Predicatul este adevărat dacă Nod este nod și nu există arcuri
%% care pornesc din Nod.


isLeaf(_):- false.

check6:-
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
	nsl('isLeaf(A)', 'A', 7)]).



%% -----------------------------------------------------------------------------
exercitiul(7, []).
%% isRoot/1
%% isRoot(?Nod)

%% Hint: Predicatul este adevărat dacă Nod este nod și nu există arcuri
%% care au ca destinație Nod.


isRoot(_):- false.

check7:-
	tests([
	chk(isRoot(a)),
	chk(isRoot(h)),
	chk(isRoot(m)),
	chk(isRoot(n)),
	uck(isRoot(b)),
	uck(isRoot(d)),
	uck(isRoot(e)),
	uck(isRoot(l)),
	nsl('isRoot(A)', 'A', 4)]).



%% -----------------------------------------------------------------------------
exercitiul(8, []).
%% descendantOf/2
%% descendantOf(?X, ?Y)
%% Nodul X este un urmaș a lui Y.

%% Hint: Predicatul este adevărat dacă există un arc de la Y la X sau
%% dacă există arc de la unul dintre urmașii lui Y la X.


descendantOf(_,_):- false.

check8:-
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
	nsl('descendantOf(f, X)', 'X', 3)]).



%% -----------------------------------------------------------------------------

exercitiul(9, []).
%% sameTree/2
%% sameTree(+Nod1, +Nod2).
%% Nod1 și Nod2 sunt în același arbore.

%% Hint: Predicatul este adevărat dacă Nod1 este urmașul lui Nod2, sau
%% Nod2 este urmașul lui Nod1, sau Nod1 și Nod2 sunt urmași ai unui
%% același nod.


sameTree(_, _):- false.

check9:-
	tests([
	chk(sameTree(a, b)),
	chk(sameTree(c, a)),
	chk(sameTree(c, e)),
	chk(sameTree(c, d)),
	chk(sameTree(n, o)),
	uck(sameTree(a, m)),
	uck(sameTree(c, n)),
	uck(sameTree(d, i))]).



%% -----------------------------------------------------------------------------
exercitiul(10, []).
%% drum/3
%% drum(?Nod1, ?Nod2, ?Drum)

%% Hint: Există un drum de la Nod1 la Nod2, dacă:
%%  - Nod1 și Nod2 sunt același nod, iar drumul este format din acel nod
%%  - Nod1 și Nod2 au un arc între ele, iar drumul este
%%  format din cele două noduri.
%%  - Nod1 este părintele unui nod X, Nod2 este un descendent al lui
%%  X, între X și Nod2 există un drum T, și atunci drumul căutat
%%  este [Nod1|T].
%%  - Nod1 este copilul unui nod X și există un drum T de la X la
%%  Node2, și atunci drumul căutat este [Nod1|T].


drum(_, _, _):- false.

check10:-
	tests([
	chk(drum(a, b, [a,b])),
	chk(drum(e, a, [e, c, a])),
	chk(drum(d, c, [d, b, a, c])),
	exp('drum(b, e, X)', ['X', [b, a, c, e]]),
	exp('drum(b, X, [b, a, c])', ['X', c]),
	exp('drum(X, b, [a, b])', ['X', a]),
	uck(drum(a, m, X)),
	uck(drum(a, X, [a, b, k]))
	]).



%% -----------------------------------------------------------------------------
exercitiul(11, []).
%% cost/3
%% cost(+Nod1, +Nod2, -Cost).
%% un arc în sus costă -1, unul în jos, 1.

%% Hint: Predicatul este adevărat dacă:
%%  - costul unui drum de la un nod la el însusi este 0.
%%  - costul unui drum de la un nod la unul dintre descendenții săi
%%  direcți este 1, iar de la un nod la părintele său este -1.
%%  - dacă Nod2 este descendent al unui copil al lui Nod1, și există un
%%  drum de cost N, de la copil la Nod2, atunci costul drumului este
%%  N+1
%%  - dacă există un drum de cost N, de la părintele lui Nod1, către
%%  Nod2, atunci costul drumului de la Nod1 la Nod2 este N-1.


cost(_, _, _):- false.

check11:-
	tests([
	chk(cost(a, a, 0)),
	chk(cost(a, b, 1)),
	chk(cost(c, a, -1)),
	chk(cost(a, d, 2)),
	uck(cost(a, h, X)),
	uck(cost(b,m,X))
	]).

