#lang racket
; ignorați următoarele linii de cod...
(define (sum numbers) (foldr + 0 numbers))
(define (number->list num) (map (lambda (c) (- (char->integer c) (char->integer #\0))) (string->list (number->string num))))

(define (count-call value) (set! call-number (add1 call-number)) value) (define (reset) (set! call-number 0)) (define call-number 0)
(define (check-calls wanted) (lambda (r) (or (eq? r 'your-code-here) (= call-number wanted) (if (= wanted 1) (format "a fost construit apelând de ~s ori în loc de o singură dată" call-number) (format "a fost construit apelând de ~s ori în loc de ~s ori" call-number wanted)))))
(define show-defaults 2) ; câte exerciții la care s-au întors rezultate default să fie arătate detaliat
(define prepend #f) (define nopoints #t) (define name-ex '(testul testele trecut exercițiul)) ; variante: '(exercițiul exercițiile rezolvat exercițiul) sau '(testul testele trecut exercițiul) sau '(task taskurile rezolvat capitolul)
(define default-results `(#f 0 () your-code-here)) (define (default-result r) (set! default-results (cons r default-results))) (define : 'separator) (define punct 'string) (define puncte 'string) (define BONUS 'string) (define exerciții 'string)
(define total 0) (define all '()) (define n-ex 0) (define p-ex 0) (define n-exercs -1) (define default-returns '()) (define (ex n sep p . s) (set! n-ex n) (set! p-ex p) (set! all (cons (list n p) all))) (define exercițiul ex) (define (sunt n s) (set! n-exercs n)) (define s-a string-append)
(define (p . L) (map (λ (e) (display e) (when (> (string-length (format "~a" e)) 0) (display " "))) L) (newline)) (define (p-n-ex) (format "[~a]" (if nopoints (string-join (list (symbol->string (cadddr name-ex)) (number->string n-ex) "/" (number->string n-exercs))) n-ex)))
(define (epart ep% pfix full) (if (< (caddr ep%) 1) (s-a pfix (if full "" (s-a (symbol->string (car name-ex)) " ")) (if (and nopoints (not full)) "" (number->string n-ex)) (symbol->string (cadr ep%))) (if (and nopoints (not full)) "" (s-a pfix (if full "" (s-a (symbol->string (car name-ex)) " ")) (number->string n-ex)))))
(define (whengood ep%) (let [(pts (* p-ex (caddr ep%)))] (and (if prepend (printf "+~v: " pts) (printf "~a[OK] " (p-n-ex))) (if nopoints (p (epart ep% "" #f) "rezolvat") (p (epart ep% "" #f) "rezolvat: +" pts (if (= pts 1) 'punct 'puncte))) (set! total (+ total pts)))))
(define (whenbad ep% gvn expcd msg) (and (when (member gvn default-results) (set! default-returns (cons (epart ep% "" #t) default-returns))) (when (or (not (member gvn default-results)) (<= (length default-returns) show-defaults)) (bad-res ep% gvn expcd msg))))
(define (bad-res ep% gvn expcd msg) (p (if prepend "+0.0:" (format "~a[--]" (p-n-ex))) (epart ep% "la " #f) 'rezultatul (format "~v" gvn) msg (if (equal? expcd '////) "" (format "~v" expcd))))
(define (check-conds e gvn conds) (or (null? conds) (let ([r ((car conds) gvn)]) (if (eq? r #t) (check-conds e gvn (cdr conds)) (whenbad e gvn '//// (or r "nu îndeplinește condiția"))))))
(define (check% part per given main-test expected . conds) (let* ([e (list n-ex part per)] [p? (pair? (cdr main-test))] [p (if p? (car main-test) identity)] [t ((if p? cadr car) main-test)] [m ((if p? cddr cdr) main-test)]) (when (eq? #t (check-conds e given conds)) (if (t (p given) expected) (whengood e) (whenbad e (p given) expected m)))))
(define (check given main-test expected . conds) (apply check% '- 1 given main-test expected conds)) (define the cons)
(define is (cons equal? "diferă de cel așteptat"))
(define in (cons member "nu se află printre variantele așteptate"))
(define same-set-as (cons (λ (x y) (apply equal? (map list->seteqv (list x y)))) "nu este aceeași mulțime cu"))
(define same-elements-as (cons (λ (x y) (and (apply = (map length (list x y))) ((car same-set-as) x y))) "nu sunt aceleași rezultate cu"))
(define (sumar) (when (and (not (null? default-returns)) (< show-defaults (length default-returns))) (p "... rezultatul implicit dat la" (cadr name-ex) (reverse default-returns))) (when (not nopoints) (p 'total: total 'puncte)))
(define (mark-helper) (printf "---~nEx  puncte    Total până aici~n") (foldr (λ (e-p t) (p (car e-p) ': (cadr e-p) "puncte. total 1 -" (car e-p) ': (+ t (cadr e-p))) (+ t (cadr e-p))) 0 all) (newline))
; ...până aici.

;; Acest laborator are ca scop familiarizarea cu legările
;; din Racket și restul limbajelor din familia LISP.

;; Cel mai important scop al acestui laborator este să vă arate cum puteți
;; face codul lizibil, țineți minte asta când rezolvați, dacă ați
;; folosit let-uri dar codul încă e greu de înțeles probabil
;; e timpul să regândiți logica exercițiului.

;; Abilitatea de a scrie cod lizibil este critică pentru un programator și vă
;; va influența major cariera. Nu vă demoralizați dacă încă nu sunteți pricepuți,
;; nu există o rețetă universală, această abilitate se dezvoltă
;; de-a lungul anilor de scris cod zilnic.

(sunt 10 exerciții)

(exercițiul 1 : 1 puncte)
;; Funcția compute-square-area primește ca argument o funcție
;; nulară ce returnează un număr, reprezentând latura unui pătrat;
;; compute-square-area trebuie să calculeze aria acelui pătrat.
;; Restricții: Aplicați get-length o singură dată.
;;             Nu puteți folosi `exp`/`expt`.
;; compute-square-area:: funcție -> real
(define (compute-square-area get-length)
  (let ((l (get-length)))
    (* l l)))

(check% 'a 1/4 (compute-square-area (lambda () (count-call 1))) is 1 (check-calls 1))(reset)
(check% 'b 1/4 (compute-square-area (lambda () (count-call 3932))) is 15460624 (check-calls 1))(reset)
(check% 'c 1/4 (compute-square-area (lambda () (count-call 2788))) is 7772944 (check-calls 1))(reset)
(check% 'd 1/4 (compute-square-area (lambda () (count-call 198.2522))) is 39303.93480483999 (check-calls 1))(reset)


(exercițiul 2 : 1 puncte)
;; Funcția compute-length primește ca argumente 3 funcții:
;;  - get-line-segment => nulară, întoarce un segment de dreaptă.
;;  - get-start-point => primește un segment de dreaptă și
;;                       întoarce punctul din care segmentul începe.
;;  - get-end-point => primește un segment de dreaptă și
;;                       întoarce punctul în care segmentul se termină.
;;
;; Un punct este reprezentat printr-o pereche de numere.
;; Un segment de dreaptă este reprezentat printr-o pereche de puncte.
;;
;; compute-length trebuie să calculeze lungimea segmentului de dreaptă.
;; (distanța dintre punctul de început și punctul de sfârșit)
;; Restricții: Fiecare funcție primită drept argument trebuie aplicată o singură dată.
;; compute-length:: funcție x funcție x funcție -> real
(define (compute-length get-line-segment get-start-point get-end-point)
  (let* ((segment (get-line-segment))
         (start (get-start-point segment))
         (stop (get-end-point segment))
         (x1 (car start)) (y1 (cdr start))
         (x2 (car stop)) (y2 (cdr stop)))
    (sqrt (+ (sqr (- x1 x2)) (sqr (- y1 y2))))))

(let ([fn-binding (lambda (x) (compute-length (lambda () (count-call x)) (lambda (s) (count-call (car s))) (lambda (s) (count-call (cdr s)))))])
  (check% 'a 1/4 (fn-binding (cons '(9 . 12) '(12 . 16))) is 5 (check-calls 3))(reset)
  (check% 'b 1/4 (fn-binding (cons '(10 . 10) '(94 . 197))) is 205 (check-calls 3))(reset)
  (check% 'c 1/4 (fn-binding (cons '(23 . 54) '(7632 . 5457))) is 9332.164272021791 (check-calls 3))(reset)
  (check% 'd 1/4 (fn-binding (cons '(658 . 665) '(32343 . 31246))) is 44035.63086864999 (check-calls 3))(reset)
  )


(exercițiul 3 : 1 puncte)
;; Definiți funcția distance care calculează distanța
;; dintre două puncte bazându-se pe funcția compute-length.
;; Identificați închiderea funcțională și arătați-o
;; asistentului pentru a vă puncta acest exercițiu.
;; distance:: pereche x pereche -> real
(define (distance x y)
  (compute-length (λ () (cons x y)) car cdr))

(check% 'a 0.25 (distance '(9 . 12)  '(12 . 16)) is 5)


(exercițiul 4 : 3 puncte)
;; Definiți funcția compute-perimeter care primește un poligon reprezentat
;; printr-o listă de puncte și calculează perimetrul acestuia.
;; Restricții: Nu folosiți funcționale.
;;             Folosiți named let.
;; Hint: Folosiți-vă de funcția distance.
;; compute-perimeter:: listă de perechi -> număr
(define (compute-perimeter points)
  (let ((start (car points)))
    (let iter ((points points) (d 0))
      (if (null? (cdr points))
          (+ d (distance (car points) start))
          (iter (cdr points) (+ d (distance (car points) (cadr points))))))))

(check% 'a 1/4 (compute-perimeter (list '(1 . 0) '(0 . 0) '(0 . 1) '(1 . 1))) is 4)
(check% 'b 1/4 (compute-perimeter (list '(2 . 0) '(0 . 0) '(0 . 2) '(2 . 2))) is 8)
(check% 'c 1/4 (compute-perimeter (list '(2 . 5) '(5 . 43) '(43 . 43))) is 132.01993654258658)
(check% 'd 1/4 (compute-perimeter (list '(2 . 2) '(4 . 5) '(0 . 3) '(4 . 3) '(3 . 1))) is 15.727968770336455)


(exercițiul 5 : 2 puncte)
;; Ne dorim să simulăm un joc simplu în doi jucători. Cei doi jucători manâncă,
;; pe rând, bomboane dintr-o grămadă ce conține `candies` bomboane.
;; Primul jucător (să îl numim "player") poate mânca doar o bomboană sau două
;; într-o singură tură. Al doilea jucător ("opponent") poate mânca exact două
;; sau trei bomboane într-o singură tură (atenție: dacă mai există o singură
;; bomboană în grămadă, nu o poate mânca).
;; Jocul se sfârșește atunci când unul din ei nu își poate efectua tura, acesta
;; pierzând. Dacă ambii joacă "inteligent", poate primul jucător să câștige?
;; Mai clar, știm că un jucător poate câștiga dacă poate mânca bomboane în
;; tura curentă și are cel puțin o "mutare" pentru care adversarul nu câștigă.
;; Hint: Modelați comportamentele celor doi prin funcții separate.
;; Restricție: Nu definiți funcții în exteriorul player-wins?. Folosiți letrec.
;; player-wins?:: întreg -> boolean
(define (player-wins? candies)
  (letrec
      ([player (lambda (candies)
                 (and (> candies 0)
                      (or (not (opponent (- candies 1)))
                          (not (opponent (- candies 2))))))]
       [opponent (lambda (candies)
                   (and (> candies 1)
                        (or (not (player (- candies 2)))
                            (not (player (- candies 3))))))])
    (player candies)))

(check% 'a 1/4 (player-wins? 2) is #t)
(check% 'b 1/4 (player-wins? 4) is #f)
(check% 'c 1/4 (player-wins? 17) is #t)
(check% 'd 1/4 (player-wins? 32) is #f)


(exercițiul 6 : 2 puncte)
;; Se dau 3 secvențe separate printr-un separator.
;; Definiți funcția `3-sequence-max` care găsește
;; suma secvenței de sumă maximă.
;; ex:
;; (1 2 3 0 3 5 4 0 5 200) cu separatorul 0
;; => secvența de sumă maximă este 205
;; Restricții: Nu folosiți fold/apply.
;;             Folosiți let-values/let*-values.
;; Hint: Uitați-vă peste splitf-at.
;;       Există deja definită în laborator funcția `sum` pentru suma elementelor unei liste.
;; 3-sequence-max:: listă de numere x orice -> număr
(define (3-sequence-max numbers separator)
  (let ([pred (lambda (x) (not (equal? x separator)))])
    (let*-values ([(left rest1) (splitf-at numbers pred)]
                  [(mid rest2) (splitf-at (cdr rest1) pred)]
                  [(right) (cdr rest2)])
      (max (sum left) (sum mid) (sum right)))))

(check% 'a 1/2 (3-sequence-max '(1 0 2 0 3) 0) is 3)
(check% 'b 1/2 (3-sequence-max '(2 3 4 0 4 105 6 0 54 5) 4) is 170)


(exercițiul 7 : 1 puncte)
;; Funcția num-concat primește două numere și le concatenează.
;; ex:
;;   > (num-concat 33 22)
;;   3322
;; Suprascrieți procedura `+` doar în contextul local pentru
;; a realiza concatenarea dintre două numere.
;; Hint: `string-append` concatenează două string-uri.
;;       Puteți folosi funcțiile `number->string` și `string->number`.
;; num-concat:: număr x număr -> număr
(define (num-concat x y)
  (let ((+ (λ (x y) (string->number (string-append (number->string x) (number->string y))))))
    (+ x y)));; Nu stergeți această linie.

(check% 'a 1/2 (num-concat 1 2) is 12)
(check% 'b 1/2 (num-concat 33 674) is 33674)


(exercițiul 8 : 2 puncte BONUS)
;; Redefiniți funcția num-concat pentru a funcționa pe oricâte numere.
;; Restricții: Nu folosiți `num-concat`.
;;             Folosiți funcționale.
;; Înțelegeți cum vă poate ajuta programarea funcțională?
;; Cum ar arăta o suprascriere echivalentă într-un limbaj procedural?
;; Care implementare este mai straightforward, este mai ușor de înțeles
;; și lasă loc pentru mai puține erori?
;; list-num-concat:: listă de numere -> număr
(define (list-num-concat numbers)
  (string->number (apply string-append (map number->string numbers))))

;; Definiți funcția care găsește toate sufixele posibile pentru un număr.
;; ex:
;; (find-all-suffixes 123) => '(123 23 3)
;; Restricții: Folosiți named let.
;;             Există deja definită în laborator funcția number->list.
;; find-all-suffixes:: număr -> listă de numere
(define (find-all-suffixes number)
  (let iter ((L (number->list number)))
    (if (null? L)
        '()
        (cons (list-num-concat L) (iter (cdr L))))))

(check% 'a 1/2 (find-all-suffixes 1234) is '(1234 234 34 4))
(check% 'b 1/2 (find-all-suffixes 56789462782) is '(56789462782 6789462782 789462782 89462782 9462782 462782 62782 2782 782 82 2))


(exercițiul 9 : 1 puncte BONUS)
;; Automatele finite sunt un formalism matematic util pentru a descrie
;; în mod abstract (matematic) procese, sunt folosite des în computer science
;; și le veți întâlni (le-ați întâlnit) la CN, LFA și alte materii.
;; În acest exercițiu vom reprezenta un automat finit prin 3 elemente:
;;  - initial-state => o stare inițială din care automatul pornește
;;  - final-state => o stare finală în care automatul se oprește
;;  - next => o funcție care primește o stare și decide care e următoarea stare.
;; Restricții: Trebuie să folosiți named let.
;; Funcția întoarce o listă ce cuprinde toate stările automatului
;; run:: stare x stare x funcție -> listă de stări
(define (run initial-state final-state next)
  (let iter ((state initial-state))
    (if (equal? state final-state)
        (list state)
        (cons state (iter (next state))))))

(check% 'a 1/4 (run 0 9 add1) is (range 10))
(check% 'b 1/4 (run 9 0 sub1) is (reverse (range 10)))
(check% 'c 1/4 (run 0 20 (compose add1 add1)) is (filter even? (range 21)))
(check% 'd 1/4 (run 2 65536 (lambda (x) (* x x))) is '(2 4 16 256 65536))


(exercițiul 10 : 2 puncte BONUS)
;; Folosindu-vă de exerciţiile anterioare generați numărul de lungime k
;; pentru care orice cifră de pe poziția i se poate obține
;; folosind formula i*k+x.
;; Cifrele sunt numerotate de la 0, pornind de la cea mai din stânga.
;; Restricții: Folosiți let.
;; generate-number:: întreg x întreg -> întreg
(define (generate-number k x)
  (let ((initial-state (+ x (* k 0)))
        (final-state   (+ x (* k (- k 1))))
        (next          (λ (x) (+ x k))))
    (list-num-concat (run initial-state final-state next))))

(check% 'a 1/2 (generate-number 3 2) is 258)
(check% 'b 1/2 (generate-number 3 3) is 369)

(sumar)
