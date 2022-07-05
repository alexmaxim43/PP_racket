#lang racket

(provide (all-defined-out))

;; Același arbore de TPP obținut în etapa 1 prin aplicarea
;; transformărilor T1, T2, T3 poate fi generat folosind 
;; tupluri GH (Gopal-Hemachandra).
;;
;; Pentru o pereche oarecare (g, e), secvența GH este:
;;    g, e, g + e, g + 2e, 2g + 3e, 3g + 5e ...
;; Pentru (g, e) = (1, 1) obținem șirul lui Fibonacci.
;;
;; Primele 4 numere din secvență formează cvartetul GH:
;;    (g, e, f, h) = (g, e, g + e, g + 2e)
;;
;; Pentru un asemenea cvartet (g, e, f, h), definim:
;;    a = gh,   b = 2ef,   c = e^2 + f^2
;; și putem demonstra că (a,b,c) este un triplet pitagoreic.
;;
;; (a,b,c) este chiar TPP, dacă adăugăm condițiile:
;;    g, e, f, h prime între ele
;;    g impar
;; însă nu veți avea nevoie să faceți asemenea verificări,
;; întrucât avem la dispoziție un algoritm care generează
;; exclusiv TPP.
;;
;; Acest algoritm este foarte asemănător cu cel din etapa
;; anterioară, cu următoarele diferențe:
;;  - nodurile din arbore sunt cvartete, nu triplete
;;    (din cvartet obținem un TPP conform formulelor)
;;    (ex: (1,1,2,3) => (1*3,2*1*2,1^2+2^2) = (3,4,5))
;;  - obținem următoarea generație de cvartete folosind 
;;    trei transformări Q1, Q2, Q3 pentru cvartete, în loc
;;    de T1, T2, T3 care lucrau cu triplete
;; 
;; Q1(g,e,f,h) = (h,e,h+e,h+2e)
;; Q2(g,e,f,h) = (h,f,h+f,h+2f) 
;; Q3(g,e,f,h) = (g,f,g+f,g+2f)
;;
;; Arborele rezultat arată astfel:
;;
;;                        (1,1,2,3)
;;              ______________|______________
;;             |              |              |
;;         (3,1,4,5)      (3,2,5,7)      (1,2,3,5)
;;       ______|______  ______|______  ______|______
;;      |      |      ||      |      ||      |      |
;;  (5,1,6,7) .........................................

;; Definim funcțiile Q1, Q2, Q3:
(define (Q1 g e f h) (list h e (+ h e) (+ h e e)))
(define (Q2 g e f h) (list h f (+ h f) (+ h f f)))
(define (Q3 g e f h) (list g f (+ g f) (+ g f f)))

;; Vom refolosi matricile T1, T2, T3:
(define T1 '((-1 2 2) (-2 1 2) (-2 2 3)))
(define T2 '( (1 2 2)  (2 1 2)  (2 2 3)))
(define T3 '((1 -2 2) (2 -1 2) (2 -2 3)))


; TODO
; Reimplementați funcția care calculează produsul scalar
; a doi vectori X și Y, astfel încât să nu folosiți
; recursivitate explicită (ci funcționale).
; Memento:
; Se garantează că X și Y au aceeași lungime.
; Ex: (-1,2,2)·(3,4,5) = -3 + 8 + 10 = 15
(define (dot-product X Y)
  ( apply + (map * X Y) )
  )


; TODO
; Reimplementați funcția care calculează produsul dintre
; o matrice M și un vector V, astfel încât să nu folosiți
; recursivitate explicită (ci funcționale).
; Memento:
; Se garantează că M și V au dimensiuni compatibile.
; Ex: |-1 2 2| |3|   |15|
;     |-2 1 2|·|4| = | 8|
;     |-2 2 3| |5|   |17|
(define (multiply M V)

  (foldl (lambda (line res) (append res (list (dot-product line V)))) null M)
  
  )


; TODO
; Aduceți aici (nu sunt necesare modificări) implementarea
; funcției get-transformations de la etapa 1.
; Această funcție nu este re-punctată de checker, însă este
; necesară implementărilor ulterioare.

(define (get-lvl nr lvl-current)
  
  (if (and (>= nr (get-range-min lvl-current)) (<= nr (get-range-max lvl-current)))
      lvl-current
      (get-lvl nr (+ lvl-current 1))
      )
  )

(define (get-range-max lvl)

  (if (= lvl 1)
      1
      (+ (expt 3 (- lvl 1)) (get-range-max (- lvl 1)))
      )
  )

(define (get-range-min lvl)

(if (= lvl 1)
    1
    (- (expt 3 (- lvl 1))(get-range-max (- lvl 1)) )
    )
  )

(define (get-treime nr min max)

  (cond
    ((and (>= nr min) (<= nr (- (+ min (quotient ( +(- max  min) 1)3)) 1)))1)
    ((and (>= nr (+ min (quotient  ( +(- max  min) 1)3)))(<= nr (- max (quotient  ( +(- max  min) 1)3))))2)
    (else 3)
    )
  )



(define (get-transformations n)

  (get-transformations-coada n '() (get-range-min (get-lvl n 1)) (get-range-max (get-lvl n 1)) )
  
  )

(define (get-transformations-coada n L min max)

  ;(get-range-min ((get-lvl n 1)))
  ;(get-range-max ((get-lvl n 1)))

  
  ( if (< (- max min) 2)
       L
       ;(get-transformations-coada n (append (list L)  (get-treime n (get-range-min (get-lvl n 1)) (get-range-max (get-lvl n 1))))  )
       (cond
         ((= (get-treime n min max ) 1) (get-transformations-coada n (append L (list 1)) min (- (+ min (quotient ( +(- max min) 1)3))1) ))
         ((= (get-treime n min max ) 2) (get-transformations-coada n (append L (list 2)) (+ min (quotient ( +(- max  min) 1)3)) (- max (quotient  ( +(- max  min) 1)3)) ) )
         (else (get-transformations-coada n (append L (list 3)) (+ ( - max (quotient  ( +(- max  min) 1)3) )1) max ))
         )
       )  
  )



; TODO
; În etapa anterioară ați implementat o funcție care primea
; o listă Ts de tipul celei întoarsă de get-transformations
; și un triplet de start ppt și întorcea tripletul rezultat
; în urma aplicării transformărilor din Ts asupra ppt.
; Acum dorim să generalizăm acest proces, astfel încât să
; putem reutiliza funcția atât pentru transformările de tip
; T1, T2, T3, cât și pentru cele de tip Q1, Q2, Q3.
; În acest scop operăm următoarele modificări:
;  - primul parametru este o listă de funcții Fs
;    (în loc de o listă numerică Ts)
;  - al doilea parametru reprezintă un tuplu oarecare
;    (aici modificarea este doar "cu numele", fără a schimba
;    funcționalitatea, este responsabilitatea funcțiilor din
;    Fs să primească parametri de tipul lui tuple)
; Nu folosiți recursivitate explicită (ci funcționale).
(define (apply-functional-transformations Fs tuple)

  (foldl (lambda (f tpl) (f tpl)) tuple Fs )
 
  )


; TODO
; Tot în spiritul abstractizării, veți defini o nouă funcție
; get-nth-tuple, care calculează al n-lea tuplu din arbore. 
; Această funcție va putea fi folosită:
;  - și pentru arborele de triplete (caz în care plecăm de la
;    (3,4,5) și avansăm via T1, T2, T3)
;  - și pentru arborele de cvartete (caz în care plecăm de la
;    (1,1,2,3) și avansăm via Q1, Q2, Q3)
; Rezultă că, în afară de parametrul n, funcția va trebui să
; primească un tuplu de start și 3 funcții de transformare a
; tuplurilor.
; Definiți get-nth-tuple astfel încât să o puteți reutiliza
; cu minim de efort pentru a defini funcțiile următoare:
;    get-nth-ppt-from-matrix-transformations
;    get-nth-quadruple
; (Hint: funcții curry)
; În define-ul de mai jos nu am precizat parametrii funcției
; get-nth-tuple pentru ca voi înșivă să decideți care este
; modul optim în care funcția să își primească parametrii.
; Din acest motiv checker-ul nu testează separat această funcție,
; dar asistentul va observa dacă implementarea respectă cerința.


;;((get-nth-tuple F1 F2 F3 Fn li) n)

(define (get-nth-tuple F1 F2 F3 Fn)

  (lambda (n)
  (foldl (lambda (x res) 
  (cond
    ((= x 1) (F1 res))
    ((= x 2) (F2 res))
    ((= x 3) (F3 res))
    ))
   Fn
       (get-transformations n))
  ))


; TODO
; Din get-nth-tuple, obțineți în cel mai succint mod posibil
; (hint: aplicare parțială) o funcție care calculează al n-lea
; TPP din arbore, folosind transformările pe triplete.
(define get-nth-ppt-from-matrix-transformations 
  ( get-nth-tuple ((curry multiply) T1) ((curry multiply) T2) ((curry multiply) T3) '(3 4 5))
  )


; TODO
; Din get-nth-tuple, obțineți în cel mai succint mod posibil 
; (hint: aplicare parțială) o funcție care calculează al n-lea 
; cvartet din arbore, folosind transformările pe cvartete.
(define get-nth-quadruple
  ( get-nth-tuple ((curry apply) Q1) ((curry apply) Q2) ((curry apply) Q3) '(1 1 2 3))
  )


; TODO
; Folosiți rezultatul întors de get-nth-quadruple pentru a 
; obține al n-lea TPP din arbore.
(define get-nth-ppt-from-GH-quadruples
  (lambda (n)
    (list (* (car (get-nth-quadruple n)) (cadddr (get-nth-quadruple n)))
          (* 2 (cadr (get-nth-quadruple n)) (caddr (get-nth-quadruple n)) )
          (+ (* (cadr (get-nth-quadruple n)) (cadr (get-nth-quadruple n))) (* (caddr (get-nth-quadruple n)) (caddr (get-nth-quadruple n))))
          )
   )
  )
;;(g, e, f, h) = (g, e, g + e, g + 2e)
;; a = gh,   b = 2ef,   c = e^2 + f^2