:- discontiguous exercitiul/2.

% Povestea (inspirată de Știrile de la Ora 5)
%
% În liniștitul nostru oraș s-a produs o crimă. Un individ a pătruns
% în casa unui bătrân și l-a ucis. Cadavrul a fost ascuns de către
% criminal și nu este de găsit. Este un caz complicat, dar doi
% detectivi, D1 și D2, fac cercetări și au deja o listă de
% suspecți. Știu despre fiecare dintre aceștia ce mașină are și care
% este arma lui preferată.
%
% Pentru a rezolva cazul trebuie să afle cu ce armă a fost ucisă
% victima și cu ce mașină a fugit criminalul. Din fericire, dacă se
% poate vorbi despre așa ceva în cazul unei crime îngrozitoare, nu
% există doi suspecți care să aibă aceeași mașină și aceeași armă.
%
% Cei doi detectivi se întâlnesc la secție. D1 s-a întâlnit cu un
% martor și a aflat cu ce mașină a fugit criminalul. D2 a găsit arma
% crimei. Cei doi se întâlnesc la secție, unde au următorul dialog pe
% care tu îl asculți indiscret.
%
% D1: Știu că nu știi cine-i criminalul. Nici eu nu știu.
% D2: Încă nu știu cine este.
% D1: Nici eu nu știu încă cine este criminalul.
% D1: Acum mi-am dat seama.
% D2: Mi-am dat seama și eu.
%
% Cine este criminalul?

% ----------------------------------------------------------------------------
% Mașini

conduce(aurel, ford).
conduce(bogdan, bmw).
conduce(cosmin, bmw).
conduce(daniel, ford).
conduce(eugen, bmw).
conduce(florin, dacia).
conduce(george, fiat).
conduce(horia, audi).
conduce(irina, dacia).
conduce(jean, fiat).
conduce(kiki, audi).
conduce(laura, seat).
conduce(marian, mercedes).
conduce(nicodim, opel).
conduce(ovidiu, honda).
conduce(paul, honda).

% Arme

inarmat(aurel, sabie).
inarmat(bogdan, pistol).
inarmat(cosmin, arbaleta).
inarmat(daniel, grenada).
inarmat(eugen, grenada).
inarmat(florin, sabie).
inarmat(george, pistol).
inarmat(horia, arbaleta).
inarmat(irina, pusca).
inarmat(jean, cutit).
inarmat(kiki, prastie).
inarmat(laura, pusca).
inarmat(marian, cutit).
inarmat(nicodim, prastie).
inarmat(ovidiu, maceta).
inarmat(paul, sabie).

% ----------------------------------------------------------------------------
% 1. Scrieți un predicat suspect(Nume:Marca:Arma) care să fie
% adevărat pentru fiecare suspect al problemei noastre.
exercitiul(1, []).

%% suspect/1
%% suspect(?Nume:?Marca:?Arma)
suspect(Nume:Marca:Arma) :- conduce(Nume, Marca), inarmat(Nume, Arma).

check1:- tests([
             exp('setof(Nume_Marca_Arma, suspect(Nume_Marca_Arma), All)', [set('All', [aurel:ford:sabie, bogdan:bmw:pistol,
             cosmin:bmw:arbaleta, daniel:ford:grenada, eugen:bmw:grenada,
             florin:dacia:sabie, george:fiat:pistol, horia:audi:arbaleta,
             irina:dacia:pusca, jean:fiat:cutit, kiki:audi:prastie,
             laura:seat:pusca, marian:mercedes:cutit, nicodim:opel:prastie,
             ovidiu:honda:maceta, paul:honda:sabie])])
          ]).

% ----------------------------------------------------------------------------
% 2. Scrieți un predicat au_pusca(-ListaNume) care să fie
% adevărat atunci când ListaNume este lista cu numele tuturor celor care
% au pușcă.
exercitiul(2, []).

%% au_pusca/1
%% au_pusca(-ListaNume)
au_pusca(ListaNume) :- findall(X, inarmat(X, pusca), ListaNume).

check2:- tests([
             exp('au_pusca(ListaNume)', [set('ListaNume', [irina, laura])])
          ]).


% ----------------------------------------------------------------------------
% 3. Scrieți predicatele au_arma(+Arma, -ListaNume)
% și au_marca(+Arma, -ListaNume), care să fie adevărate atunci când
% ListaNume este lista cu numele tuturor celor care au arma de tipul
% Arma, respectiv mașina de tipul Marca.
exercitiul(3, []).

%% au_arma/2
%% au_arma(+Arma, -ListaNume)
au_arma(Arma, ListaNume) :- findall(X, inarmat(X, Arma), ListaNume).

%% au_marca/2
%% au_marca(+Arma, -ListaNume)
au_marca(Marca, ListaNume) :- bagof(X, conduce(X, Marca), ListaNume).

check3:- tests([
             exp('au_arma(pistol, Pistolari)', [set('Pistolari', [bogdan, george])]),
             exp('au_arma(cutit, Cutitari)', [set('Cutitari', [jean, marian])]),
             exp('au_arma(maceta, Macelari)', [set('Macelari', [ovidiu])]),
             exp('au_marca(bmw, NuSemnalizeaza)', [set('NuSemnalizeaza', [bogdan, cosmin, eugen])]),
             exp('au_marca(dacia, ConducDacie)', [set('ConducDacie', [florin, irina])]),
             exp('au_marca(seat, ConducSeat)', [set('ConducSeat', [laura])]),
             exp('findall(_, au_marca(X,Y), L)',  [cond(length(L, 9))])
          ]).

% ----------------------------------------------------------------------------
% 4. Scrieți un predicat arme_bmw(ListaArme) care să fie adevărat
% atunci când ListaArme reprezintă mulțimea tuturor armelor deținute
% de conducători de bmw.
exercitiul(4, []).

%% arme_bmw/1
%% arme_bmw(-ListaArme)
arme_bmw(ListaArme) :- setof(Arma, N^suspect(N:bmw:Arma), ListaArme).
% dar merge și findall(Arma, suspect(_:bmw:Arma), ListaArme).

check4:- tests([
             exp('arme_bmw(Arme)', [set('Arme', [arbaleta, grenada, pistol])])
          ]).


% ----------------------------------------------------------------------------
% 5. Scrieți un predicat arme_marca(Marca, ListaArme) care să
% fie adevărat atunci când ListaArme reprezintă mulțimea tuturor
% armelor deținute de conducători de mașini de tipul Marca.
exercitiul(5, []).

%% arme_marca/2
%% arme_marca(+Marca, -ListaArme)
arme_marca(Marca, ListaArme) :- setof(Arma, N^suspect(N:Marca:Arma), ListaArme).
% dar merge și findall(Arma, suspect(_:Marca:Arma), ListaArme).

check5:- tests([
             exp('arme_marca(bmw, ArmeBmw)', [set('ArmeBmw', [arbaleta, grenada, pistol])]),
             exp('arme_marca(dacia, ArmeDacia)', [set('ArmeDacia', [pusca, sabie])]),
             exp('arme_marca(seat, ArmeSeat)', [set('ArmeSeat', [pusca])]),
             exp('findall(Y, arme_marca(X,Y), [ _ , L | _ ])', [set('L', [arbaleta, grenada, pistol])])
          ]).

% ----------------------------------------------------------------------------
% 6. Scrie un predicat marci_arma_unica(ListaMarci) care să afișeze
% lista mașinilor pentru care lista armelor pe care le dețin
% conducătorii unei mărci conține un singur element. Hint: folosiți-vă
% de rezolvarea exercițiului 5. Nu folosiți length/2.
exercitiul(6, []).

%% marci_arma_unica/1
%% marci_arma_unica(-ListaMarci)
marci_arma_unica(ListaMarci) :-
    % toate mărcile care indică o singură armă (toți cei care conduc
    % mașina de marca M folosesc o aceeași armă
    findall(M, (conduce(_, M), arme_marca(M, [_])), ListaMarci).

check6:- tests([
             exp('marci_arma_unica(ListaMarci)', [set('ListaMarci', [mercedes, opel, seat])])
          ]).

% ----------------------------------------------------------------------------
% ----------------------------------------------------------------------------

% Să revenim la secție de poliție unde tu tragi cu urechea la dialogul
% dintre cei doi detectivi:
%
% Prima replică:
% Detectiv A : Știam că nu știi cine-i criminalul.
%
% Ce înseamnă asta? Detectivul A știe mașina cu care a fugit
% suspectul. Această marcă de mașină este condusă de suspecți care
% mânuiesc diferite arme. Dacă măcar una dintre aceste arme ar fi
% aparținut doar unui singur suspect, atunci Detectivul B ar fi putut
% ști care este soluția acestui caz.
%
% Ce soluții eliminăm?
%
% Dacă Detectivul A ar fi aflat că adevăratul criminal a condus o
% Honda, atunci ar fi existat două soluții posibile:
%
% ovidiu - honda - maceta
% paul - honda - sabie
%
% Dar cum nu există decât un singur individ care are macetă (Ovidiu),
% Detectivul A nu ar fi putut afirma că Detectivul B nu poate ști.
%
% honda nu este, deci, o soluție
%
% Trebuie eliminate toate mașinile care sunt "în pereche" cu arme
% pentru care nu există mai multe posibilități.

% ----------------------------------------------------------------------------
% 7. Scrie un predicat suspect1/1 care este adevărat doar pentru
% numele suspecților care respectă condiția impusă de replica
% Detectivului A: niciuna dintre armele asociate cu mașina suspectului
% nu indică în mod unic un anumit individ.
exercitiul(7, []).

%% suspect1/1
%% suspect1(?Nume:?Marca:?Arma)
suspect1(Nume:Marca:Arma) :-
    suspect(Nume:Marca:Arma),
    arme_marca(Marca, Arme),
    % pentru fiecare din armele asociate cu Marca,
    % sunt cel puțin doi suspecți care au arma respectivă.
    forall(member(A, Arme), au_arma(A, [_,_ |_])).

check7:- tests([
             exp('setof(Nume_Marca_Arma, suspect1(Nume_Marca_Arma), All)',
            [set('All',
            [aurel:ford:sabie,bogdan:bmw:pistol,cosmin:bmw:arbaleta,
             daniel:ford:grenada,eugen:bmw:grenada,florin:dacia:sabie,
             george:fiat:pistol,horia:audi:arbaleta,irina:dacia:pusca,
             jean:fiat:cutit,kiki:audi:prastie,laura:seat:pusca,
             marian:mercedes:cutit,nicodim:opel:prastie])])
          ]).

% ----------------------------------------------------------------------------
% A doua replică:
%
% Detectivul A: Nici eu nu știu!
%
% 8. Scrie un predicat suspect2/1 care este adevărat doar pentru
% numele suspecților care respectă condiția impusă de replica
% Detectivului A: marca nu indică unic un individ.
%
% Atenție: informația ce trebuie filtrată acum este cea care
% corespunde primei replici.
exercitiul(8, []).

%% suspect2/1
%% suspect2(?Nume:?Marca:?Arma)
suspect2(Nume:Marca:Arma) :-
    suspect1(Nume:Marca:Arma),
    % sunt cel puțin 2 suspecți pentru care predicatul suspect1 este adevărat
    % și au mașina pe care o cunoaște detectivul A.
    findall(N, suspect1(N:Marca:_), [_,_ |_]).

check8:- tests([
             exp('setof(Nume_Marca_Arma, suspect2(Nume_Marca_Arma), All)',
            [set('All',
            [aurel:ford:sabie,bogdan:bmw:pistol,cosmin:bmw:arbaleta,
              daniel:ford:grenada,eugen:bmw:grenada,florin:dacia:sabie,
              george:fiat:pistol,horia:audi:arbaleta,irina:dacia:pusca,
              jean:fiat:cutit,kiki:audi:prastie])])
          ]).

% ----------------------------------------------------------------------------
% A treia replică:
%
% Detectivul B: Nici eu nu știu!
%
% 9. Scrie un predicat suspect3/1 care este adevărat doar pentru
% numele suspecților care respectă condiția impusă de replica
% Detectivului B: arma nu identifică unic un individ.
%
% Atenție: informația ce trebuie filtrată acum este cea care
% corespunde primelor două replici.
exercitiul(9, []).

%% suspect3/1
%% suspect3(?Nume:?Marca:?Arma)
suspect3(Nume:Marca:Arma) :-
    suspect2(Nume:Marca:Arma),
    % sunt cel puțin 2 suspecți pentru care predicatul suspect2 este adevărat
    % și au arma pe care o cunoaște detectivul B.
    findall(N, suspect2(N:_:Arma), [_,_ |_]).

check9:- tests([
             exp('setof(Nume_Marca_Arma, suspect3(Nume_Marca_Arma), All)',
            [set('All',
            [aurel:ford:sabie,bogdan:bmw:pistol,cosmin:bmw:arbaleta,
              daniel:ford:grenada,eugen:bmw:grenada,florin:dacia:sabie,
              george:fiat:pistol,horia:audi:arbaleta])])
          ]).

% ----------------------------------------------------------------------------
% A patra replică:
%
% Detectivul A: Eu tot nu știu!
%
% 10. Scrie un predicat suspect4/1 care este adevărat doar pentru
% numele suspecților care respectă condiția impusă de replica
% Detectivului A.
%
% Atenție: informația ce trebuie filtrată acum este cea care
% corespunde primelor trei replici.
exercitiul(10, []).

%% suspect4/1
%% suspect4(?Nume:?Marca:?Arma)
suspect4(Nume:Marca:Arma) :-
    suspect3(Nume:Marca:Arma),
    % sunt cel puțin 2 suspecți pentru care predicatul suspect3 este adevărat
    % și au mașina pe care o cunoaște detectivul A.
    findall(N, suspect3(N:Marca:_), [_,_ |_]).

check10:- tests([
             exp('setof(Nume_Marca_Arma, suspect4(Nume_Marca_Arma), All)',
            [set('All',
            [aurel:ford:sabie,bogdan:bmw:pistol,cosmin:bmw:arbaleta,
              daniel:ford:grenada,eugen:bmw:grenada])])
          ]).

% ----------------------------------------------------------------------------
% A cincea replică:
%
% Detectivul B: Eu am aflat!
%
% 11. Scrie un predicat suspect5/1 care este adevărat doar pentru
% numele suspecților care respectă condiția impusă de replica
% Detectivului B.
%
% Atenție: informația ce trebuie filtrată acum este cea care
% corespunde primelor patru replici.
exercitiul(11, []).

%% suspect5/1
%% suspect5(?Nume:?Marca:?Arma)
suspect5(Nume:Marca:Arma) :-
    suspect4(Nume:Marca:Arma),
    % este un singur suspect pentru care predicatul suspect4 este adevărat
    % și are arma pe care o cunoaște detectivul B.
    % (practic, selectăm suspecții cu arme care indică unic un suspect).
    findall(N, suspect4(N:_:Arma), [_]).

check11:- tests([
             exp('setof(Nume_Marca_Arma, suspect5(Nume_Marca_Arma), All)',
            [set('All',
            [aurel:ford:sabie,bogdan:bmw:pistol,cosmin:bmw:arbaleta])])
          ]).

% ----------------------------------------------------------------------------
% A șasea replică:
%
% Detectivul A: Și eu am aflat!
%
% 12. Scrie un predicat suspect6/1 care este adevărat doar pentru
% numele suspecților care respectă condiția impusă de replica
% Detectivului A.
%
% Atenție: informația ce trebuie filtrată acum este cea care
% corespunde primelor cinci replici.
exercitiul(12, []).

%% suspect6/1
%% suspect6(?Nume:?Marca:?Arma)
suspect6(Nume:Marca:Arma) :-
    suspect5(Nume:Marca:Arma),
    % este un singur suspect pentru care predicatul suspect5 este adevărat
    % și are mașina pe care o cunoaște detectivul A.
    % (practic, selectăm suspecții cu mașini care indică unic un suspect).
    findall(N, suspect5(N:Marca:_), [_]).

check12:- tests([
             exp('setof(Nume_Marca_Arma, suspect6(Nume_Marca_Arma), All)',
            [set('All',
            [aurel:ford:sabie])])
          ]).
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

