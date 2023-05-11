%% Prolog - Legare - execuție
:- discontiguous exercitiul/2.
:- ensure_loaded('checker.pl').
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
%% indică plasarea simbolului S la coordonatele X, Y.
%%
%% Hint: ansamblul X/Y/S este valid în raport cu cele din lista others
%% dacă nu există o altă poziționare a aceluiași simbol pe aceeași linie
%% sau pe aceeași coloană.
safe(_, _) :- false.


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


check2 :- tests([
              chk(permutari4([1,2,3,4],[1,2,3,4])),
              chk(permutari4([1,2,3,4],[2,1,4,3])),
              uck(permutari4([1,2,3,4],[2,2,4,3])),
              uck(permutari4([1,2,3,4],[2,2,4,3,1])),
              nsl("permutari4([1,2,3,4], P)", 'P', 24)
          ]).

%% -- CONSTRUCȚIA SOLUȚIILOR MULTIPLE FOLOSIND ALTERNATIVE --

%% -----------------------------------------------------------------------------
exercitiul(3, []).
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

check3 :- tests([
              chk(sublista([1,2,3,4,5],[2,3,4])),
              chk(sublista([1,2,3,4,5],[1,2,3,4,5])),
              uck(sublista([1,2,3,4,5],[2,1,4])),
              sls('sublista([1,2,3],S4)', 'S4',
                  [[],[1],[1,2],[1,2,3],[1,3],[2],[2,3],[3]])
          ]).


exercitiul(4, []).
%% intersectie/3
%% intersectie(+L1, +L2, -L3)
%% 'L3' reprezintă o listă construită prin intersectarea listelor L1 și L2
%%
%% Hint: folosiți findall, unde verificați dacă un element din L1 se află în L2.

intersectie(_, _, _) :- false.

check4 :- tests([
    chk(intersectie([1,2,3,4,5],[2,3,4], [2, 3, 4])),
    chk(intersectie([1,2,3,4,5],[1,2,3,4,5], [1, 2, 3, 4, 5])),
    uck(intersectie([1,2,3,4,5],[2,1,4], [1, 2])),
    uck(intersectie([1,2,3,4,5],[3, 4, 5, 6, 7], [3, 4, 5, 6])),
    sls('intersectie([1,2,3,4],[2,3,4,5],S4)', 'S4',
        [[2,3,4]])
]).

exercitiul(5, []).
%% diff/3
%% diff(+L1, +L2, -L3)
%% 'L3' reprezintă o listă construită prin diferența dintre listele L1 și L2 (L1 / L2)
%%
%% Hint: folosiți findall, unde verificați dacă un element din L1 NU se află în L2.

diff(_, _, _) :- false.

check5 :- tests([
    chk(diff([1,2,3,4,5],[2,3,4], [1, 5])),
    chk(diff([1,2,3,4,5],[1,2,3,4,5], [])),
    uck(diff([1,2,3,4,5],[2,1,4], [1, 2])),
    uck(diff([1,2,3,4,5],[3, 4, 5, 6, 7], [1, 2, 7])),
    sls('diff([1,2,3,4,6],[2,3,4,5],S4)', 'S4',
        [[1, 6]])
]).


exercitiul(6, []).
%% countBetween/4
%% countBetween(+L, +A, +B, -N)
%% 'N' reprezintă numărul de elemente din lista L care sunt mai mari sau egale cu A
%% și mai mici sau egale cu B
%%
%% Hint: folosiți findall, unde generați o listă care conține elementele din lista L
%% care respectă condiția menționată mai sus, și funcția length pe lista generată folosind findall

countBetween(_, _, _, _) :- false.

check6 :- tests([
    chk(countBetween([1,2,3,4,5,6,7,8,9], 2, 9, 8)),
    chk(countBetween([1,2,3,4,5], 1, 5, 5)),
    uck(countBetween([1,2,3,4,5], 2, 4, 2)),
    uck(countBetween([1,2,3,4,5,6,7,8,9], 2, 10, 10)),
    sls('countBetween([1,2,3,4,5,6,7,8,9], 4, 9, S4)', 'S4',
        [6])
]).

exercitiul(7, []).
%% subset/2
%% subset(+L1, +L2)
%% 'subset' reprezintă un predicat care verifică dacă toate elementele din lista L1
%% se află în lista L2.
%%
%% Hint: folosiți forall, unde verificați că toate elementele din L1 se află în L2

subset(_, _) :- false.

check7 :- tests([
    chk(subset([1,3,2], [1, 2, 3, 4, 5, 6, 7, 8, 10])),
    chk(subset([1,2,3,4,5], [1, 2, 3, 4, 5, 6, 7, 8, 10])),
    uck(subset([0,1,2,3,4,5], [1, 2, 3, 4, 5, 6, 7, 8, 10]))
]).

exercitiul(8, []).
%% minList/2
%% minList(+L, -M)
%% 'minList' reprezintă un predicat care determină pentru o listă L minimul acesteia (M)
%%
%% Hint: folosiți forall, prin care verificați dacă toate elementele din L sunt mai mari
%% sau egale cu minimul generat (aici folosiți predicatul 'member').

minList(_, _) :- false.

check8 :- tests([
    chk(minList([1,2,3,4,5,6,7,8,9], 1)),
    chk(minList([5,6,7,8,9], 5)),
    uck(minList([0,1,2,3,4,5,6,7,8,9], 1)),
    sls('minList([6,7,8,9], S4)', 'S4',
        [6])
]).

%% -- DIRECȚIONALITATE FLEXIBILĂ ÎN CALCUL --

%% -----------------------------------------------------------------------------
exercitiul(9, []).
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

check9 :- tests([
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


exercitiul(10, []).
%% auCopil/2
%% auCopil(?A, ?B)
%% Predicatul exprimă faptul că două persoane au un copil împreună.
%%
%% Hint: persoanele trebuie să fie diferite.

auCopil(_, _) :- false.

check10 :- tests([
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

exercitiul(11, []).
%% soti/2
%% soti(?A, ?B)
%% Predicatul arată că două persoane sunt soți. Considerăm că sunt soți
%% dacă au un copil împreună. Folosiți predicatul cut, astfel încât:
%%   - o pereche să nu apară de mai multe ori
%%   - să avem exact 3 soluții pentru interogarea ?- soti(X, Y).
%%
%% Hint: folosirea lui cut într-un predicat face ca apelul
%% acelui predicat să întoarcă o singură soluție. Deci nu putem folosi
%% cut în predicatul soti. În ce predicat deja implementat, echivalent
%% altfel cu soti, puteți folosi cut, și la care nu se cere să returneze
%% mai multe soluții, ci poate fi folosit doar pentru verificarea că cei
%% doi au copii împreună?
%%
%% Hint: cei doi trebuie să fie persoane.
%%
%% Hint: puteți folosi operatorul de ordonare naturală @< pentru a
%% impune o anumită ordine între persoane.
soti(_, _) :- false.


check11 :- tests([
              chk((soti(mihai, wendy) ; soti(wendy, mihai))),
              chk((soti(gigi, rodica) ; soti(rodica, gigi))),
              chk((soti(radu, miruna) ; soti(miruna, radu))),
              uck(soti(mihai, rodica)),
              uck(soti(rodica, rodica)),
              uck(soti(ana, alin)),
              uck(soti(miruna, alin)),
              nsl("soti(X, Y)", "(X, Y)", 3)
          ]).
