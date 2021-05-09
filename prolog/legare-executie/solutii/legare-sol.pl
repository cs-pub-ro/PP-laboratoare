
:- discontiguous exercitiul/2.
%% -----------------------------------------------------------------------------

%% -- PUTEREA GENERATIVĂ A LIMBAJULUI --
exercitiul(1, []).
%% În problema pătratului latin, trebuie ca să nu existe același simbol
%% de mai multe ori pe aceeași linie sau pe aceeași coloană.

%% Înțelegeți predicatele solve_latin/1, template/1 și correct/1.
%% Observați că lipsește definiția predicatului safe/2.

%% template/1
%% template(?List)
%% List are forma unei soluții pentru problema pătratului latin.
%% Lungimea soluției este cunoscută și fixă.
%% 'Template'-ul este o listă cu toate cele 9 poziții, fiecare poziție o
%% alipire (un 'compound') dintre coordonatele poziției și simbolul de
%% pe poziția respectivă. În template, simbolurile nu sunt legate încă.
template([1/1/_, 1/2/_, 1/3/_, 2/1/_, 2/2/_, 2/3/_, 3/1/_, 3/2/_, 3/3/_]).

%% correct/1
%% correct(?Solution)
%% Solution reprezintă o soluție validă pentru problemă. Într-o soluție
%% validă fiecare poziție este validă în raport cu următoarele.
correct([]).
correct([X/Y/S | Others]):-
        correct(Others),
        member(S, [a, b, c]),
        safe(X/Y/S, Others).

%% solve_latin/1
%% solve_latin(-Solution)
%% Solution este o soluție a problemei pătratului latin.
solve_latin(S) :- template(S), correct(S).

%% Scrieți predicatul safe/2 utilizat în rezolvarea problemei.
%% Predicatul verifică dacă plasarea simbolului S pe coloana X
%% și linia Y este validă în raport cu lista Others. Aceasta
%% are forma [X1/Y1/S1, X2/Y2/S2 ...].

%% TODO
%% safe/2
%% safe(+X/Y/S, +Others)
%% Unde Others este lista pozițiilor deja completate, iar compusul X/Y/S
%% indică plasarea simbolului S pe
%%
%% Hint: ansamblul X/Y/S este valid în raport cu cele din lista others
%% dacă nu există o altă poziționare a aceluiași simbol pe aceeași linie
%% sau pe aceeași coloană.
safe(_, _) :- false.
safe(X/Y/S, Others) :-
    \+ member(X/_/S, Others), \+ member(_/Y/S, Others).

check1 :- tests([
              uck(safe(1/1/a, [1/3/a])),
              uck(safe(1/2/b, [3/2/b])),
              chk(safe(2/2/b, [1/1/b, 3/3/b, 2/1/a, 2/3/c, 1/2/a])),
              chk(safe(1/1/a, [1/2/b, 1/3/c, 2/1/c, 2/2/a, 2/3/b, 3/1/b, 3/2/c, 3/3/a])),
              uck(safe(3/2/b, [1/2/b, 1/3/c, 2/1/c, 2/2/a, 2/3/b, 3/1/b, 3/3/a])),
              chk(safe(3/2/c, [1/1/a, 1/2/b, 1/3/c, 2/1/c, 2/2/a, 2/3/b, 3/1/b, 3/3/a])),
              nsl('(Sol=[1/1/a, 1/2/b, 1/3/c, 2/1/c, 2/2/a, 2/3/b, 3/1/_, 3/2/_, 3/3/_],
                        solve_latin(Sol))', 'Sol', 1),
              nsl('(Sol=[1/1/a, 1/2/b, 1/3/c, 2/1/_, 2/2/_, 2/3/_, 3/1/_, 3/2/_, 3/3/_],
                        solve_latin(Sol))', 'Sol', 2),
              nsl('solve_latin(Sol)', 'Sol', 12)
          ]).

%% Întrebați-l pe Prolog "solve_latin(Sol)" pentru a vizualiza
%% soluțiile.



%% -----------------------------------------------------------------------------
exercitiul(2, []).
%% permutari4/2
%% permutari(+Lista4, -Permutare4)
%% Predicatul calculează permutările unei liste de 4 elemente.
%%
%% Hint: dăm proprietățile permutării: permutarea are aceeași lungime și
%% fiecare dintre elementele din Lista4 sunt și în permutare.
permutari4([_, _, _, _], _) :- false.
permutari4([A, B, C, D], Perm) :- length(Perm, 4),
    member(A, Perm), member(B, Perm), member(C, Perm), member(D, Perm).

check2 :- tests([
              chk(permutari4([1,2,3,4],[1,2,3,4])),
              chk(permutari4([1,2,3,4],[2,1,4,3])),
              uck(permutari4([1,2,3,4],[2,2,4,3])),
              uck(permutari4([1,2,3,4],[2,2,4,3,1])),
              nsl("permutari4([1,2,3,4], P)", 'P', 24)
          ]).

%% -----------------------------------------------------------------------------
exercitiul(3, []).
%% alternare/4
%% alternare(+A, +B, +Len, -Res)
%% Predicatul construiește în Res o listă de lungime Len în care se
%% alternează valorile A și B.
%%
%% Observați outputul de deasupra testului c;
%% Observați cum variabilele sunt legate *după* construcția listei în
%% interogarea:
%% alternare(X, Y, 5, Res), writeln(Res), X = 1, Y = 2.
alternare(_, _, _, _) :- false.
alternare(_, _, 0, []).
alternare(A, B, Len, [A | Res]) :- L is Len - 1, Len > 0,
    alternare(B, A, L, Res).

check3 :- tests([
              exp("alternare(X, Y, 10, [A, B, C, D | Rest])", [
                      cond("length(Rest, 6)"),
                      cond("X == A"), cond("C == A"),
                      cond("Y == B"), cond("D == B")
                  ]),
              ech("(alternare(X, Y, 10, Res), member(E, Res))",
                  ['var(E)', '(X == E ; Y == E)']),
              wait, chk((alternare(X, Y, 4, Res),
                   format("~n     A: ~w, B: ~w, Res: ~w~n", [X, Y, Res])))
          ]).


%% -- CONSTRUCȚIA SOLUȚIILOR MULTIPLE FOLOSIND ALTERNATIVE --

%% -----------------------------------------------------------------------------
exercitiul(4, []).
%% sublista/2
%% sublista(+List, ?SubList)
%% 'SubList' este o sublistă a lui 'List' ('SubList' poate fi obținută prin
%% eliminarea a zero sau mai multe elemente din 'List')
%%
%% Hint: în parcurgerea listei, primul element din listă poate să facă
%% parte din sublistă (ca prim element al sublistei), sau poate să nu
%% facă parte din sublistă. Restul sublistei este o sublistă din restul
%% listei.

sublista(_,_) :- false.
sublista([], []).
sublista([H|T], [H|SubT]) :- sublista(T, SubT).
sublista([_|T], SubT) :- sublista(T, SubT).

check4 :- tests([
              chk(sublista([1,2,3,4,5],[2,3,4])),
              chk(sublista([1,2,3,4,5],[1,2,3,4,5])),
              uck(sublista([1,2,3,4,5],[2,1,4])),
              sls('sublista([1,2,3],S4)', 'S4',
                  [[],[1],[1,2],[1,2,3],[1,3],[2],[2,3],[3]])
          ]).


exercitiul(5, []).
%% extract/4
%% extract(+List, +Start, +End, -Range)
%% 'Range' reprezintă lista formată din elementele listei 'List' aflate
%% pe pozițiile din intervalul 'Start' .. 'End'. 'Start' va fi întotdeauna
%% mai mic decât 'End'. Primul element se află pe poziția 0. Dacă 'End'
%% depășește lungimea listei, 'Range' va conține toate elementele de la 'Start'
%% la finalul listei.
%%
%% Hint: pe măsură ce avansăm în listă, decrementăm Start și End. Avem
%% mai multe cazuri:
%%    - lista s-a terminat - nu mai avem ce extrage
%%    - am ajuns la sfârșitul zonei de extras (End este 0)
%%    - nu am ajuns încă la începutul zonei de extras (Start > 0) -
%%    continuăm mai departe fără să adăugăm elemente în range
%%    - suntem în interiorul zonei de extras (Start = 0 și End > 0) -
%%    mergem în listă și adăugăm elemente în Range.

extract(_,_,_,_) :- false.
extract([], _, _, []).
extract(_, _, 0, []).
extract([H|T], 0, End, [H|Range]) :-
    End1 is End-1,
    extract(T, 0, End1, Range).
extract([_|T], Start, End, Range) :-
    S1 is Start - 1, E1 is End - 1,
    extract(T, S1, E1, Range).

check5 :- tests([
              chk(extract([a,b,c,d,e], 0, 3, [a,b,c,d])),
              chk(extract([a,b,c,d,e], 1, 10, [b,c,d,e])),
              exp('extract([a,b,C,D,e], 2, 4, [c,d,e])', ['C', c, 'D', d]),
              exp('extract([a,b,C1,D1,e], 2, 4, [C2,D2,e])',
                  [cond('C1==C2'), cond('D1==D2')]),
              chk(extract([a,b,c,_,d], 3, 4, [X,X]))
          ]).



%% -- DIRECȚIONALITATE FLEXIBILĂ ÎN CALCUL --

%% -----------------------------------------------------------------------------
exercitiul(6, []).
%% factorial2/2
%% factorial2(?N, ?Fact)
%% 'Fact' este factorialul lui 'N'.
%% Cel puțin unul dintre cele două argumente trebuie să fie legat la un
%% număr, dar oricare dintre ele poate să nu fie legat.
%%
%% Hint: pentru ca o asociere număr-factorial să existe, trebuie să
%% existe o altă asociere număr-anterior - factorial-anterior, pe baza
%% căreia putem calcula numărul și factorialul actual.

factorial2(_, _) :- false.
factorial2(0, 1).
factorial2(N, F) :- factorial2(N1, F1), N is N1 + 1, F is F1 * N.

check6 :- tests([
              exp('factorial2(2, F1)', ['F1', 2]),
              exp('factorial2(4, F2)', ['F2', 24]),
              exp('factorial2(N3, 120)', ['N3', 5]),
              exp('factorial2(N4, 720)', ['N4', 6]),
              chk(factorial2(7, 5040))
          ]).


%% -- CONTROLUL NUMĂRULUI SOLUȚIILOR ȘI PREDICATUL Cut !/0 --

%% -----------------------------------------------------------------------------

% Considerăm următorul arbore genealogic:
%
% mihai --- wendy  rodica --- gigi
%	 |                 |
%    ana    radu  ---  miruna
%                  |
%	     ioana   alin

% parinte/2
% parinte(?X, ?Y)
% Y este parintele lui X
% X este copilul lui Y
parinte(ioana, radu).
parinte(ioana, miruna).
parinte(alin, radu).
parinte(alin, miruna).
parinte(ana, mihai).
parinte(ana, wendy).
parinte(radu, mihai).
parinte(radu, wendy).
parinte(miruna, rodica).
parinte(miruna, gigi).

% lista completă de persoane:
persoane([alin, ana, gigi, ioana, mihai, miruna, radu, rodica, wendy]).


exercitiul(7, []).
%% auCopil/2
%% auCopil(?A, ?B)
%% Predicatul exprimă faptul că două persoane au un copil împreună.
%%
%% Hint: persoanele trebuie să fie diferite.

auCopil(_, _) :- false.
auCopil(A, B) :- parinte(X, A), parinte(X, B), A \= B, !.

check7 :- tests([
              chk(auCopil(mihai, wendy)),
              chk(auCopil(rodica, gigi)),
              chk(auCopil(radu, miruna)),
              uck(auCopil(mihai, mihai)),
              uck(auCopil(mihai, rodica)),
              uck(auCopil(rodica, rodica)),
              uck(auCopil(ioana, alin)),
              uck(auCopil(ana, alin)),
              uck(auCopil(miruna, alin))
          ]).

exercitiul(8, []).
%% soti/2
%% soti(?A, ?B)
%% Predicatul arată că două persoane sunt soți. Considerăm că sunt soți
%% dacă au un copil împreună. Folosiți predicatul cut, astfel încât:
%%   - o pereche să nu apară de mai multe ori
%%   - să avem exact 3 soluții pentru interogarea ?- soti(X, Y).
%%
%% Hint: folosirea lui cut într-un predicat face ca apelul
%% acelui predicat să întoarcă o singură soluție. Deci nu putem folosi
%% cur în predicatul soti. În ce predicat deja implementat, echivalent
%% altfel cu soti, puteți folosi cut, și la care nu se cere să returneze
%% mai multe soluții, ci poate fi folosit doar pentru verificarea că cei
%% doi au copii împreună?
%%
%% Hint: cei doi trebuie să fie persoane.
%%
%% Hint: puteți folosi operatorul de ordonare naturală @< pentru a
%% impune o anumită ordine între persoane.
soti(_, _) :- false.
soti(A, B):- persoane(L),
    member(A, L),
    member(B, L),
    A @< B,
    auCopil(A, B).

check8 :- tests([
              chk((soti(mihai, wendy) ; soti(wendy, mihai))),
              chk((soti(gigi, rodica) ; soti(rodica, gigi))),
              chk((soti(radu, miruna) ; soti(miruna, radu))),
              uck(soti(mihai, rodica)),
              uck(soti(rodica, rodica)),
              uck(soti(ana, alin)),
              uck(soti(miruna, alin)),
              nsl("soti(X, Y)", "(X, Y)", 3)
          ]).

exercitiul(9, []).
%% linie/2
%% linie(+Urmas, -Linie_Genealogică)
%% Predicat pentru aflarea unei linii genealogice pentru o persoană.
%% Aceasta începe cu persoana (Urmașul) și continuă cu toți strămoșii
%% săi, până la o persoană ai cărei parinți nu sunt cunoscuți.
%% Soluții succesive ale predicatului prezintă diversele linii
%% genealogice existente pentru urmaș.
linie(_, _) :- false.
linie(Urmas, [Urmas]) :- \+ parinte(Urmas, _).
linie(Urmas, [Urmas | Cale]) :-
    parinte(Urmas, Parinte),
    linie(Parinte, Cale).

check9 :- tests([
              chk(linie(ioana, [ioana, radu, mihai])),
              chk(linie(ioana, [ioana, radu, wendy])),
              chk(linie(ioana, [ioana, miruna, rodica])),
              chk(linie(ioana, [ioana, miruna, gigi])),
              sls("linie(alin, L)", "L",
                  [[alin, radu, mihai], [alin, radu, wendy],
                   [alin, miruna, rodica], [alin, miruna, gigi]]),
              sls("linie(ana, L)", "L",
                  [[ana, mihai], [ana, wendy]]),
              sls("linie(rodica, L)", "L", [[rodica]])
          ]).

exercitiul(10, []).
%% prima_linie_prin/3
%% prima_linie_prin(+Urmas, +Persoana, -Linie_Genealogică)
%% Ne dorim să aflăm o singură linie genealogică, prima în care este
%% prezentă o anumită persoană.
prima_linie_prin(_, _, _) :- false.
prima_linie_prin(Urmas, Persoana, Linie) :-
    linie(Urmas, Linie),
    member(Persoana, Linie), !.

check10 :- tests([
               chk(prima_linie_prin(alin, miruna, [alin, miruna, rodica])),
               exp("prima_linie_prin(alin, miruna, L)",
                   ['L', [alin, miruna, rodica]]),
               nsl("prima_linie_prin(alin, miruna, L)", 'L', 1),
               uck(prima_linie_prin(alin, ana, _))
           ]).


exercitiul(11, []).
%% stramosi/2
%% stramosi(+Urmas, -Stramosi)
%% Predicatul determină toți strămoșii pentru un anumit urmaș.
%%
%% Hint: o persoană are exact 2 părinți, care fiecare are strămoșii lui.
%% Folosiți predicatul cut la momentul / momentele oportun(e) pentru a
%% avea o singură soluție.
stramosi(_, _) :- false.
stramosi(Urmas, [Urmas]) :- \+ parinte(Urmas, _), !.
stramosi(Urmas, [Urmas | Stramosi]) :-
    parinte(Urmas, A),
    parinte(Urmas, B),
    A \= B, !,
    stramosi(A, SA),
    stramosi(B, SB),
    append(SA, SB, Stramosi).

check11 :- tests([
               exp("stramosi(ioana,X)", [
                   set('X', [ioana, radu, mihai, wendy, miruna, rodica, gigi])]),
               exp("stramosi(ana,X)", [
                   set('X', [ana, mihai, wendy])]),
               exp("stramosi(rodica,X)", [set('X', [rodica])]),
               nsl("stramosi(ioana,X)", 'X', 1),
               nsl("stramosi(ana,X)", 'X', 1)
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
