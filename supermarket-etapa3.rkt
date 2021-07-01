#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

;; ATENȚIE: Pentru această etapă a temei este necesar să implementați
;;          întâi TDA-ul queue în fișierul queue.rkt.
;; Reveniți la sarcinile din acest fișier după ce ați implementat tipul 
;; queue și ați verificat implementarea folosind checker-ul.


; Structura counter nu se modifică.
; Ceea ce se modifică este implementarea câmpului queue:
; - în loc de listă, acesta va fi o coadă (o structură de tip queue)
; - acest lucru nu este vizibil în definiția structurii counter,
;   ci în implementarea operațiilor acestui tip de date (tipul counter)
(define-struct counter (index tt et queue) #:transparent)


; TODO
; Actualizați funcțiile de mai jos astfel încât ele să folosească
; o structură de tip queue pentru reprezentarea cozii de persoane.
; Elementele cozii continuă să fie perechi (nume . nr_produse).
; Este esențial să respectați "bariera de abstractizare", adică să
; operați cu coada doar folosind interfața acestui TDA:
; - empty-queue
; - queue-empty?
; - enqueue
; - dequeue
; - top
; Obs: Doar câteva funcții vor necesita actualizări.

;; pentru empty-counter:
(define (empty-counter index)           ; testată de checker
  (define C (make-counter index 0 0 empty-queue))C)  
  ;;'your-code-here)


;; pentru update:
(define (update-help f counters counters-partial index)
  (if (null? counters) counters-partial
      (if (= (counter-index (car counters)) index) (update-help f (cdr counters) (append counters-partial (list (f (car counters)))) index)
          (update-help f (cdr counters) (append counters-partial (list (car counters))) index))))

(define (update f counters index)
 (update-help f counters null index))


;; pentru tt+:
(define tt+
  (λ (minutes)
    (λ (C)
      (match C
    [(counter index tt et queue)
     (make-counter index (+ tt minutes) et queue)]))))

;; pentru et+:
(define et+
  (λ (minutes)
    (λ (C) 
    (match C
    [(counter index tt et queue)
     (make-counter index tt (+ et minutes) queue)]))))

;; pentru add-to-counter:
(define (add-to-counter name items)     ; testată de checker
  (λ (C)                                ; nu modificați felul în care funcția își primește argumentele
    (match C
    [(counter index tt et queue)
     (if (queue-empty? queue) (make-counter index (+ tt items) (+ et items) (enqueue (cons name items) queue))
         (make-counter index (+ tt items) et (enqueue (cons name items) queue)))])))

;; pentru functie abstracta:
(define (counter-prop-help f counters counters-partial)
   (cond
    ((null? counters) counters-partial)
    ((<= (cdr counters-partial) (f (car counters))) (counter-prop-help f (cdr counters) counters-partial))
    (else
     (counter-prop-help f (cdr counters) (cons (counter-index (car counters)) (f (car counters)))))))

(define (counter-prop f counters)
  (counter-prop-help f counters (cons (counter-index (car counters)) (f (car counters)))))


(define (min-tt counters)
  (counter-prop counter-tt counters))


(define (min-et counters)
  (counter-prop counter-et counters))

;; pentru remove:
(define (calcul-tt queue)
  (if (queue-empty? queue) 0
      (+ (cdr (top queue)) (calcul-tt (dequeue queue)))))

(define (queue-length queue)
  (+ (queue-size-l queue) (queue-size-r queue)))


(define (remove-first-from-counter C)   ; testată de checker
  (if (= (queue-length (counter-queue C)) 1) (struct-copy counter C [tt 0] [et 0] [queue empty-queue])
      (struct-copy counter C [tt (calcul-tt (dequeue (counter-queue C)))] [et (cdr (top (dequeue (counter-queue C))))] [queue (dequeue (counter-queue C))]))); 'your-code-here)

; TODO
; Implementați o funcție care calculează starea unei case după un număr dat de minute.
; Funcția presupune, fără să verifice, că în acest timp nu a ieșit nimeni din coadă, 
; deci va avea efect doar asupra câmpurilor tt și et.
; (cu alte cuvinte, este responsabilitatea utilizatorului să nu apeleze această funcție
; cu minutes > timpul până la ieșirea primului client din coadă)
; Atenție: casele fără clienți nu trebuie să ajungă la timpi negativi!
(define (pass-time-through-counter minutes)
  (λ (C)
    (match C
    [(counter index tt et queue)
     (if(< minutes tt)
        (make-counter index (- tt minutes) (- et minutes) queue)
        (make-counter index 0 0 queue))])))

; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 2, apar modificări în:
; - formatul listei de cereri (parametrul requests)
; - formatul rezultatului funcției (explicat mai jos)
; requests conține 4 tipuri de cereri (3 moștenite din etapa 2 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă            (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute       (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)         (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                     (   NOU!   )
; Obs: Au dispărut cererile de tip remove-first, pentru a lăsa loc unui mecanism mai 
; sofisticat de a scoate clienții din coadă (pe măsură ce trece timpul).
; Sistemul trebuie să proceseze cele 4 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)  (ca înainte)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți) (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele (și cele fast, și cele slow), 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>       (ca înainte)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică.
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista caselor în starea finală (ca rezultatul din etapele 1 și 2)
; Obs: Pentru a contoriza ieșirile din cozi, puteți să lucrați într-o funcție ajutătoare
; (cu un parametru în plus față de funcția serve), pe care serve doar o apelează.
(define (serve requests fast-counters slow-counters)
   ;(if (null? requests) (append fast-counters slow-counters)
    ;  (match (car requests)
     ;   [(list 'ensure average)
       ;  (if (> (average-aux (total-time counter-tt (append fast-counters slow-counters)) (length (append fast-counters slow-counters))) average)
      ;          (serve requests fast-counters (append slow-counters (list (empty-counter (add1 (length (append fast-counters slow-counters)))))))
        ;        (serve (cdr requests) fast-counters slow-counters))]
        ;
        ;[(list name n-items) (if (<= n-items ITEMS) (if (<= (car (min-tt (append fast-counters slow-counters))) (length fast-counters))
         ;                                              (serve (cdr requests) (update (add-to-counter name n-items) fast-counters (car (min-tt (append fast-counters slow-counters)))) slow-counters)
          ;                                             (serve (cdr requests) fast-counters (update (add-to-counter name n-items) slow-counters (car (min-tt (append fast-counters slow-counters))))))
           ;                      (serve (cdr requests) fast-counters (update (add-to-counter name n-items) slow-counters (car (min-tt slow-counters)))))]
        
       ; [(list 'delay index minutes)
        ; (serve (cdr requests)
         ;       (update (et+ minutes) (update (tt+ minutes) fast-counters index) index)
          ;      (update (et+ minutes) (update (tt+ minutes) slow-counters index) index))])))
  'your-code-here)










(define (valid-counters-help f counters counters-partial)
  (cond
    ((null? counters) counters-partial)
    ((null? (f (car counters))) (valid-counters-help f (cdr counters) counters-partial))
    (else (valid-counters-help f (cdr counters) (append counters-partial (list (car counters)))))))

(define (valid-counters f counters)
  (valid-counters-help f counters null))

(define (total-time-help f counters partial-time)
  (if (null? counters)
      partial-time
      (total-time-help f (cdr counters) (+ partial-time (f (car counters))))))

(define (total-time f counters)
  (total-time-help f counters 0))

(define (average-aux counters-total-time counters-number)
  (/ counters-total-time counters-number))
