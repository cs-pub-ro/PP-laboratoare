# Prolog: Legare și execuție
  * Data publicării: 09.05.2021
  * Data ultimei modificări: 09.05.2021

## Obiective

Scopul acestui laborator este introducerea unor noțiuni mai avansate de Prolog:
  * procesul de backtracking realizat de Prolog
  * lucrul cu variabile neinstanțiate (nelegate)
  * controlul execuției și predicatul cut.


== Puterea generativă a limbajului ==

Așa cum am văzut în laboratorul precedent scopurile pot fi privite ca întrebări ale căror răspunsuri sunt ''true'' sau ''false''. În plus, acest răspuns poate fi însoțit de instanțierile variabilelor din cadrul scopului. Acest mecanism ne ajută să folosim scopurile pentru a obține rezultate de orice formă.

De exemplu, pentru a obține lungimea unei liste putem folosi:
<code prolog>
% lungime(+Lista,-Lungime)
lungime([],0).
lungime([_ | R], N):- lungime(R, N1), N is N1 + 1.
</code>

<code prolog>
?- lungime([1,2,3],N).
N = 3.
</code>

În exemplul de mai sus se va încerca satisfacerea scopului ''lungime([1,2,3],N).'' prin instanțierea convenabilă a variabilei ''N''. În acest caz soluția este unică, dar așa cum am văzut anterior, putem avea situații în care există mai multe unificări posibile. Putem folosi faptul că se încearcă resatisfacerea unui scop în mod exhaustiv pentru a genera multiple rezultate.

În exemplul de mai jos vom defini predicatul **membru(?Elem,+List)** care verifică apartenența unui element la o listă:

<code prolog>
% membru(?Elem,+Lista)
membru(Elem, [Elem | _]).
membru(Elem, [_ | Rest]) :- membru(Elem, Rest).
</code>

Putem folosi acest predicat pentru a obține un răspuns:
<code prolog>
?- membru(3, [1, 2, 3, 4, 5]).
true .
</code>

Sau putem să îl folosim pentru a genera pe rând toate elementele unei liste:

<code prolog>
?- membru(N, [1, 2, 3]).
N = 1 ;
N = 2 ;
N = 3 ;
false.
</code>

Inițial, scopul ''membru(N, [1,2,3]).'' va unifica cu faptul ''membru(Elem, [Elem | _]).'', în care  ''Elem = N'' și ''Elem = 1'', din care rezultă instanțierea ''N = 1''. Apoi se va încerca unificarea cu antetul de regulă ''membru(Elem, [_ | Rest])'', în care ''Elem = N'', iar ''Rest = [2, 3]''. Acest lucru implică satisfacerea unui nou scop, ''membru(N, [2, 3]).''. Noul scop va unifica, de asemenea, cu faptul de la linia 1, ''membru(Elem, [Elem | _]).'', din care va rezulta ''N = 2''. Asemănător se va găsi și soluția ''N = 3'', după care nu va mai reuși nicio altă unificare.

Pentru a exemplifica utilizarea acestui mecanism, vom considera următorul exemplu în care dorim generarea, pe rând, a tuturor permutărilor unei liste:

<code prolog>
% remove(+Elem, +Lista, -ListaNoua)
remove(E, [E | R], R).
remove(E, [F | R], [F | L]) :- remove(E, R, L).

% perm(+Lista, -Permutare)
perm([], []).
perm([F | R], P) :- perm(R, P1), remove(F, P, P1).
</code>

Observați ca am definit predicatul **remove(+Elem, +Lista, -ListaNoua)**, care șterge un element dintr-o listă. Rolul acestuia în cadrul regulii ''perm([F | R], P):- perm(R, P1), remove(F, P, P1).'' este, de fapt, de a insera elementul ''F'' în permutarea ''P1''. Poziția pe care va fi inserat elementul va fi diferită la fiecare resatisfacere a scopului ''remove(F, P, P1)'', ceea ce ne ajută sa obținem permutările.


<code prolog>
?- remove(3, L, [1, 1, 1]).
L = [3, 1, 1, 1] ;
L = [1, 3, 1, 1] ;
L = [1, 1, 3, 1] ;
L = [1, 1, 1, 3] ;
false.
</code>


<code prolog>
?- perm([1, 2, 3], Perm).
Perm = [1, 2, 3] ;
Perm = [2, 1, 3] ;
Perm = [2, 3, 1] ;
Perm = [1, 3, 2] ;
Perm = [3, 1, 2] ;
Perm = [3, 2, 1] ;
false.
</code>


Putem folosi puterea generativă a limbajului pentru a produce soluții bazate pe combinații. De exemplu, dacă ne dorim toate perechile de numere (ca soluții succesive) din listele ''L1'' și ''L2'', dar a căror sumă **nu** este în lista ''L3'', putem folosi interogarea:

<code prolog>
[L1,L2,L3]=[[1,2,3], [4,5,6], [5,6]], member(X, L1), member(Y, L2), S is X + Y, \+ member(S, L3).
</code>


== Obținerea de soluții prin generare și testare ==

Fie problema colorării a 7 țări de pe o hartă folosind 3 culori. Scopul este acela de a atribui câte o culoare fiecărei țări, astfel încât nicio țară să nu aibă niciun vecin de aceeași culoare cu aceasta. Soluția problemei va fi o listă de atribuiri din domeniul ''["r", "g", "b"]'', care desemnează culorile atribuite fiecărei țări ''(1, 2, 3, 4, 5, 6, 7)''.

Această strategie se traduce în următorul cod Prolog:
<code>
all_members([], _). % predicat care verifică că toate elementele din prima listă sunt prezente în a doua
all_members([X | Rest], In) :- member(X, In), all_members(Rest, In).
solve(S) :-
	L = [_ | _], length(L, 7), all_members(L, ["r", "g", "b"]),
	safe(S). % predicat care verifică faptul că țările nu au culori identice cu niciun vecin
</code>
	
Programul anterior este foarte ineficient. El construiește extrem de multe atribuiri, fără a le respinge pe cele "ilegale" într-un stadiu incipient al construcției. 


== Backtracking atunci când cunoaștem dimensiunea soluției ==

Mecanismul de backtracking ne oferă o rezolvare mai eficientă. Știm că orice soluție pentru problema colorării hărților constă într-o listă de atribuiri a 3 culori, de genul ''%%[X1/C1,X2/C2, ... X7/C7]%%'', scopul programului de rezolvare fiind să instanțieze adecvat variabilele X1, C1, X2, C2 etc.

Vom considera că orice soluție este de forma ''%%[1/C1,2/C2, ... 7/C7]%%'', deoarece ordinea țărilor nu este importantă. 

Fie problema mai generală a colorării a N țări folosind M culori. Definim soluția pentru N = 7 ca o soluție pentru problema generală a colorării hărților, care în plus respectă template-ul ''%%[1/Y1,2/Y2, ... 7/Y7]%%''. Semnul "/" este folosit în acest caz ca o modalitate de alipire a unor valori, fără a calcula vreodată împărțirea.

În Prolog vom scrie:
<code>
%% Lungimea soluției este cunoscută și fixă.
template([1/_, 2/_, 3/_, 4/_, 5/_, 6/_, 7/_]).

correct([]) :- !.
correct([X/Y | Others]):-
        correct(Others),
        member(Y, ["r", "g", "b"]),
        safe(X/Y, Others).

solve_maps(S):-template(S), correct(S).
</code>

**Regulă:** Atunci când calea către soluție respectă un anumit template (avem de instanțiat un număr finit, predeterminat, de variabile), este eficient să definim un astfel de template în program. 

**Observație:** În exemplul de mai sus am reținut explicit ordinea celor 7 țări. Redundanța în reprezentarea datelor ne asigură un câștig în viteza de calcul (câștigul se observă la scrierea predicatului safe).


== Controlul execuției: operatorul cut (!), negația (\+) și false

=== Negația ca eșec (\+) ===

''\+'' este operatorul folosit pentru negație în Prolog (meta-predicatul ''not'' nu mai este recomandat). Așa cum ați observat nu se pot adăuga în baza de date fapte în forma negată și nici nu se pot scrie reguli pentru acestea. Dacă nu se poate demonstra ''¬p'', atunci ce semnificație are în Prolog ''not(Goal)'' sau ''\+ Goal''?

Prolog utilizează //presupunerea lumii închise//: ceea ce nu poate fi demonstrat, este fals. De aceea, în Prolog ''\+ p'' trebuie citit ca "scopul ''p'' nu poate fi satisfăcut" sau "''p'' nu poate fi demonstrat". Faptul că Prolog utilizează negația ca eșec (eng. //negation as falseure//) are implicații asupra execuției programelor.

În logica de ordinul întâi, următoarele două expresii sunt echivalente: ''¬a(X) & b(X)'' și ''b(X) & ¬a(X)''. În Prolog, următoarele 2 clauze (''p1'' și ''p2'') vor produce rezultate diferite:

<code Prolog>
student(andrei).
student(marius).
lazy(marius).

p1(X) :- student(X), \+ lazy(X).
p2(X) :- \+ lazy(X), student(X).
</code>

Acest lucru se întâmplă pentru că, în ''p2'', Prolog nu poate să derive, pe baza negației, legări pentru ''X''. În prolog putem folosi negația doar pentru a //verifica// variabile deja legate, sau pentru a exprima faptul că //nu se poate demonstra că predicatul este adevărat//. În ''p1'', ''X'' este legat și negația are rolul de a verifica că ''lazy'' nu este adevărat pentru ''X''. În ''p2'', ''X'' este nelegat, deci putem interpreta rezultatele folosind a doua modalitate: Prolog va încerca să demonstreze că nu există ''X'' pentru care ''lazy'' să fie adevărat, ceea ce nu este corect.


=== Predicatul false ===
''false'' (echivalent cu ''false'', dar adăugat mai de curând pentru o interpretare mai clară) este un predicat care eșuează întotdeauna. De ce am vrea să scriem o regulă care să eșueze? Un posibil răspuns este: datorită efectelor pe care le pot avea scopurile satisfăcute până la întâlnirea predicatului ''false''. Un exemplu este următorul:

<code Prolog>
my_reverse(List, Acc, _) :- format('List:~w, Acc:~w~n', [List, Acc]), false.
my_reverse([], Sol, Sol).
my_reverse([Head | Tail], Acc, Sol):-my_reverse(Tail, [Head | Acc], Sol).
</code>

În codul de mai sus, am scris o regulă pentru funcția ''my_reverse'' care are un singur rol: acela de a afișa argumentele ''List'' și ''Acc'' în orice moment se dorește satisfacerea unui scop cu predicatul ''my_reverse/3''. Regula însă nu va avea efect asupra restului execuției, din moment ce ea eșuează întotdeauna.

<code Prolog>
?- my_reverse([1,2,3,4],[],Rev).
List:[1,2,3,4], Acc:[]
List:[2,3,4], Acc:[1]
List:[3,4], Acc:[2,1]
List:[4], Acc:[3,2,1]
List:[], Acc:[4,3,2,1]
Rev = [4, 3, 2, 1].
</code>

=== Operatorul cut ===

În Prolog, operatorul cut (''!'') are rolul de a elimina toate punctele de bifurcație create în predicatul curent. La evaluarea predicatului cut într-un predicat ''p'', se vor realiza două tipuri de efecte:
  * nu se vor mai genera soluții (dacă este nevoie, sau dacă soluția curent eșuează) pentru alte reguli ale predicatului ''p''
  * nu se vor mai genera soluții (dacă este nevoie, sau dacă soluția curent eșuează), pentru alte soluții ale condițiilor care apar **în aceeași regulă cu cut**, și înainte de cut.
  
De exemplu, în programul:
<code Prolog>
p(a).
p(b).
p(A/B) :- q(A), !, t(A/B).
p(d).

q(a).
q(b).
q(c).

t(a/a).
t(a/b).
t(b/c).
t(b/d).
</code>

Interogarea ''?- p(X).'' va întoarce soluțiile:
<code Prolog>
X = a ;
X = b ;
X = a/a ;
X = a/b.
</code>

Primele două soluții sunt soluții date de primele două reguli pentru ''p''. Pentru următoarea soluție, Prolog încearcă regula cu cut. Pentru ''q(A)'', sunt trei alternative: ''A=a, A=b, A=c''. La încercarea primeia, însă, urmează predicatul cut, care anulează:
  * alternativele pentru ''q'', deci nu se vor mai considera posibilitățile ''A=b'' și ''A=c'';
  * alternativele pentru ''p'', deci nu se va mai considera regula ''p(d)''.
Alternativele pentru ''t'' sunt însă considerate normal, pentru că acestea se creează //după// evaluarea lui cut.

Putem utiliza predicatul cut în două moduri:
  * atunci când știm că am ajuns la soluția care ne interesează, și știm că nu mai avem nevoie de o altă soluție pentru predicat, putem utiliza cut pentru a nu mai explora alte soluții (cut verde / //green cut//).
  * atunci când dorim în mod explicit ca Prolog să nu mai exploreze alte posibilități pentru același predicat, pentru că acestea nu ar genera soluții corecte, dacă se aplică regula curentă (cut roșu / //red cut//).

Exemplu: implementarea predicatului ''min''.

Varianta 1 -- fără cut:
min(X, Y, Min) :- X < Y, X = Min. % regula 1
min(X, Y, Min) :- X >= Y, Y = Min. % regula 2

Este important să avem comparația din regula 2, în lipsa ei obținând, pentru cazul în care ''X < Y'', o a doua soluție falsă, în care Y este minimul. Testați cu:

minB(X, Y, Min) :- X < Y, X = Min. % regula 1
minB(_, Y, Min) :- Y = Min. % regula 2

Pentru interogarea ''minB(2, 3, Min)'' se obțin două soluții: ''Min=2'' și ''Min=3''.

Putem integra predicatul cut ca un cut verde astfel:

min2(X, Y, Min) :- X < Y, !, X = Min. % regula 1
min2(X, Y, Min) :- X >= Y, Y = Min. % regula 2

Este o utilizare de tip "green cut" - cut poate să lipsească și execuția nu devine incorectă, dar este îmbunătățită prin faptul că atunci când ''X < Y'' Prolog va ști să nu mai intre (nu mai este nevoie să intre) pe a doua regulă.

Observați că este important ca predicatul cut să fie pus atunci când știm cu siguranță ca suntem pe ramura corectă. Dacă, de exemplu, implementăm astfel:

min2B(X, Y, Min) :- !, X < Y, X = Min.
min2B(X, Y, Min) :- X >= Y, Y = Min.

Pentru ''min2B(3, 2 ,M)'', se va evalua predicatul cut (care anulează alternativa pentru min2B), inegalitatea eșuează, și interogarea va eșua, pentru că din cauza lui cut Prolog nu mai intră și pe a doua regulă.


Putem integra predicatul cut ca un cut roșu astfel:

min3(X, Y, Min) :- X < Y, !, X = Min.
min3(_, Y, Min) :- Y = Min.

Dacă inegalitatea din prima regulă reușește, sigur nu mai avem nevoie de a doua regulă.

Aici, cut **nu** poate să lipsească -- dacă lipsește vom obține și soluții incorecte, ca în cazul lui ''minB''.

