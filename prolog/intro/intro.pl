%% LABORATOR 09
%% Prolog - Intro
:- discontiguous exercitiul/2.
%% -----------------------------------------------------------------------------

%% Pentru a testa laboratorul, folositi check. Pentru a testa exercitiul
%% N, folositi checkN.

exercitiul(1, []).
%% myConcat/3
%% myConcat(?List1, ?List2, ?List)
%% 'List' este lista formată prin concatenarea listelor 'List1' și
%% 'List2' si functioneaza similar cu un parametru acumulator.

%% Hint: Predicatul myConcat este adevărat dacă primul element al
%% rezultatului (List) este egal cu primul element al listei List1, iar
%% restul lui List este egal cu concatenarea restului lui List1 cu
%% List2.

myConcat(_,_,_):- fail.

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
%% myReverse(?List, +RevList)
%% 'RevList' este o listă ce conține elementele listei 'List' în ordine inversă.
%% Regulile pot conține și predicatul myConcat/3.

%% Hint: Predicatul este adevărat dacă RevList are ca ultim element
%% primul element din L, iar prima parte a lui RevList (de la primul
%% element pâna la penultimul) este inversul restului elementelor din L.

myReverse(_,_):- fail.

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

%% Hint: Inversul listei se va construi in Acc. Predicatul este adevărat
%% dacă valoarea primului element al acumulatorului, pentru restul lui
%% List, este egal cu primul element din List.



check3:-
    tests([
        1, chk(myReverseAcc([], [], [])),
        1, chk(myReverseAcc([1,2,3], [0], [3,2,1,0])),
        2, exp('myReverseAcc([1,2,3], [0], Rev)', ['Rev', [3,2,1,0]]),
        2, exp('myReverseAcc(List, [0], [3,2,1,0])', ['List', [1,2,3]]),
        2, exp('myReverseAcc([X2,1], [3], [X1,2,3])', ['X1', 1, 'X2', 2])]).
myReverseAcc(_,_,_):- fail.

exercitiul(4, []).
%% factorial/2
%% factorial(+N, -Fact)
%% 'Fact' este factorialul lui 'N'.
%% N va fi mereu legat la un număr natural.

%% Hint: Predicatul este adevărat dacă F este egal cu N*(N-1)!.

factorial(_, _):- fail.

check4:-
	tests([
        exp('factorial(1, F1)',['F1',1]),
	exp('factorial(4, F2)',['F2',24]),
        chk(factorial(5, 120)),
	chk(factorial(6, 720)),
	chk(factorial(7, 5040))]).


%% -----------------------------------------------------------------------------
exercitiul(5,[]).
%% palindrom/2
%% palindrom(+List)
%% 'List' este un palindrom.

%% Hint: Predicatul este adevărat dacă inversul lui List este List.

palindrom(_):- fail.

check5 :-
	tests([
	chk(palindrom([1,2,3,2,1])),
	chk(palindrom([1,2,3,3,2,1])),
	uck(palindrom([1,2,3,0,2,1])),
	exp('palindrom([1,2,3,X3,X2,X1])', ['X1', 1, 'X2', 2, 'X3', 3]),
	uck(palindrom([1,2,3,X,_,X]))]).


%% -----------------------------------------------------------------------------

exercitiul(6,[]).
%% setIntersection/3
%% setIntersection(+L1, +L2, -L)
%% L este intersectia listelor L1 si L2.

%% Hint:
%% - În cazul în care primul element din L1 (H1) este în L2,
%% predicatul este adevărat dacă H1 se află pe prima pozitie în lista
%% rezultat.
%% - În cazul în care primul element din L1 nu este în L2, predicatul
%% este adevărat dacă L3 reprezintă intersectia dintre restul lui L1 si
%% L2.

setIntersection(_, _, _):- fail.

check6:-
    tests([
        1, chk(setIntersection([], [1,2], [])),
        1, chk(setIntersection([1,2,3], [7,9,24], [])),
        1, chk(setIntersection([1,2,3], [2], [2])),
        2, exp('setIntersection([1,2,3,4,7,13], [7,13,21], Int)', ['Int', [7,13]])]).

%% -----------------------------------------------------------------------------

exercitiul(7,[]).
%% setDiff/3
%% setDiff(+L1, +L2, -L)
%% L este diferenta listelor L1 si L2 (L1 - L2)

%% Hint:
%% - În cazul în care primul element din L1 (H1) nu este în L2,
%% predicatul este adevărat dacă H1 se află pe prima pozitie în lista
%% rezultat.
%% - În cazul în care primul element din L1 este în L2, predicatul
%% este adevărat dacă L3 reprezintă diferenta dintre restul lui L1 si
%% L2.


setDiff(_, _, _):- fail.

check7:-
    tests([
        1, chk(setDiff([], [1,2], [])),
        1, chk(setDiff([1,2,3], [7,9,24], [1,2,3])),
        1, chk(setDiff([1,2,3], [2], [1,3])),
        2, exp('setDiff([1,2,3,4,7,13], [7,13,21], Diff)', ['Diff', [1,2,3,4]])]).


%% -----------------------------------------------------------------------------

exercitiul(8,[]).
%% setUnion/3
%% setUnion(+L1, +L2, -L)
%% L este reuniunea listelor L1 si L2.

%% Hint: Predicatul este adevărat dacă L este egal cu
%% L1 ++ (L2 - (L1 intersectat L2))



setUnion(_, _, _):- fail.

check8:-
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
exercitiul(9, []).
%% isLeaf/1
%% isLeaf(?Nod)

%% Hint: Predicatul este adevărat dacă Nod este nod si nu există arcuri
%% care pornesc din Nod.

isLeaf(_):- fail.

check9:-
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
exercitiul(10, []).
%% isRoot/1
%% isRoot(?Nod)

%% Hint: Predicatul este adevărat dacă Nod este nod si nu există arcuri
%% care au ca destinatie Nod.

isRoot(_):- fail.

check10:-
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
exercitiul(11, []).
%% descendantOf/2
%% descendantOf(?X,?Y)
%% Nodul X este un urmaș a lui Y.

%% Hint: Predicatul este adevărat dacă există un arc de la Y la X sau
%% dacă există arc de la unul din urmasii lui Y la X.


descendantOf(_,_):- fail.

check11:-
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

exercitiul(12, []).
%% sameTree/2
%% sameTree(+Nod, +Nod).

%% Hint: Predicatul este adevărat dacă Nod1 este urmasul lui Nod2, sau
%% Nod2 este urmasul lui Nod1, sau Nod1 si Nod2 sunt urmasi ai aceleiasi
%% rădăcini.

sameTree(_, _):- fail.


check12:-
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
exercitiul(13, []).
%% drum/3
%% drum(?Nod, ?Nod, ?Lista)

%% Hint: Predicatul este adevărat dacă:
%%  - există drum de la un nod la el însusi si drumul este format din
%%  acel nod
%%  - există drum de la un nod A la un nod B dacă există arc de la A la
%%  B sau de la B la A si drumul este format din cele două noduri.
%%  - dacă există un drum T, de la unul din descendentii direct ai lui A
%%  la B, iar drumul dintre A si B este [A|T].
%%  - dacă există un drum T de la unul din strămosii directi ai lui A la
%%  B, iar drumul dintre A si B este [A|T].

drum(_, _, _):- fail.


check13:-
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
exercitiul(14, []).
%% cost/3
%% cost(+Nod, +Nod, -Cost).
%% un arc in sus costa -1, unul in jos, 1.

%% Hint: Predicatul este adevărat dacă:
%%  - costul unui drum de la un nod la el însusi este 0.
%%  - costul unui drum de la un nod la unul din descendentii directi
%%  este 1, iar de la un nod la unul din ascendentii directi este -1.
%%  - dacă există un drum de cost N, de la unul din descendentii directi
%%  ai lui A la B si costul drumului de la A la B este N+1.
%%  - dacă există un drum de cost N, de la unul din stramosii directi
%%  ai lui A la B si costul drumului de la A la B este N-1.

cost(_, _, _):- fail.

check14:-
	tests([
	chk(cost(a, a, 0)),
	chk(cost(a, b, 1)),
	chk(cost(c, a, -1)),
	chk(cost(a, d, 2)),
	uck(cost(a, h, X)),
	uck(cost(b,m,X))
	]).


%% ----------------------------------------
%% ----------------------------------------
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
