%% Observați diferențele dintre unificare și testarea echivalenței:

% ?- X = Y.
% X = Y.

% ?- X == Y.
% false.

%% Observați diferențele dintre ==/2, =/2, =:=/2 și is/2
%% (echivalență, unificare și evaluari aritmetice).

% ?- X = 1 + 2.
% X = 1+2.

% ?- X == 1 + 2.
% false.

% ?- X is 1 + 2.
% X = 3.

% ?- X =:= 1 + 2.
% ERROR: =:=/2: Arguments are not sufficiently instantiated


%% Predicatele pot fi adevărate sau false, ele nu pot întoarce valori.
%% Rezultatul obținut va fi unul din argumentele predicatului.
%% Pentru claritate, antetele predicatelor se scriu sub forma:
%% modulo(+X,+Y,-Z)
%% pentru a diferenția intrarile (+) de iesiri(-).
%% Acele argumente care pot fi fie intrări, fie ieșiri se prefixează cu '?'.

modulo(X,Y,X) :- X<Y.
modulo(X,Y,Z) :- X>=Y, X1 is X-Y, modulo(X1,Y,Z).

%% Dacă folosim cut, se va incerca satisfacerea celei de-a
%% doua reguli doar atunci cand X<Y este fals, deci nu mai
%% este nevoie să specificăm ca X>=Y, precum în cazul anterior.
%% Predicatul cut ne poate ajuta la optimizarea codul scris
%% de noi, împiedicând resatisfacerea anumitor predicate.

modulo1(X,Y,X) :- X<Y,!.
modulo1(X,Y,Z) :- X1 is X-Y, modulo1(X1,Y,Z).

%% membru(?Elem,+Lista)
%% Observați ce se intampla daca cereti membru(X,[a,b,c]).
%% Pentru a satisface scopul membru(X,[a,b,c]), se încearcă
%% legarea variabilei X la o valoare. Deoarece procesul nu
%% se oprește până când scopul nu este satisfăcut în toate
%% modurile posibile, X se va lega pe rând la a, b și c.
%% Aceasta este puterea generativă a limbajului.
%% Observați că dacă am fi scris prima regulă sub forma:
%% membru(Elem,[Elem|_]):-!.
%% nu ar mai fi avut același comportament.

membru(Elem,[Elem|_]).
membru(Elem,[_|Rest]):- membru(Elem,Rest).

%% Verifică daca o lista este sortată.
%% e_sortata(+Lista).

e_sortata([ ]).
e_sortata([_]).
e_sortata([X,Y|Rest]):- X=<Y, e_sortata([Y|Rest]).

%% Inserarea unui element într-o listă sortată.
%% Observați cum lucrăm asupra rezultatului prin forma pe care i-o dam
%% în antetul regulii.
%% ins(+Elem,+Lista,-Rez)

ins(X,[],[X]).
ins(X,[Y|R],[X,Y|R]):- X=<Y,!.
ins(X,[Y|R],[Y|R1]):- ins(X,R,R1).

%% Calculează lungimea unei liste în două moduri.
%% Observați cum putem construi rezultatul la întoarcerea
%% din recursivitate sau intrând în recursivitate, folosind
%% un acumulator, ce inițial va fi 0.
%% lungime(+Lista,-Lungime)

lungime([],0).
lungime([_|R], N):- lungime(R,N1), N is N1 + 1.

%% Observați că atunci când am terminat de parcurs lista,
%% lungimea acesteia este egale cu cea calculată în acumulator.
%% lungime(+Lista,+Acumulator,-Lungime)

lungime2([],N,N).
lungime2([_|R],N,M) :-  N1 is N +1, lungime2(R,N1,M).

%% Determina al N-lea numar fibonacci.
%% fibonacci(+N,-Fibo)
fibonacci(N,N):- N =< 1, !.
fibonacci(N,F):- N1 is N -1, N2 is N - 2,
        fibonacci(N1,F1),
        fibonacci(N2,F2),
        F is F1 + F2.


%% Șterge un element din lista.
%% Observati cum lucram asupra rezultatului prin forma pe care i-o dam
%% în antetul regulii.
%% Încercați să apelati remove(1,X,[2,3]).
%% remove(+Elem,+Lista,-ListaNoua)

remove(E,[E|R],R).
remove(E,[F|R],[F|L]):- remove(E,R,L).

%% Determină permutări ale listei.
%% Le dă pe rand, la cerere (nu toate într-o listă).
%% Observați cum ne folosim de remove pentru a genera,
%% la fiecare resatisfacere a sa, o permutare P în care elementul
%% F este inserat pe altă poziție în P1.
%% perm(+Lista,-Permutare)

perm([],[]).
perm([F|R],P):- perm(R,P1), remove(F,P,P1).
