#lang racket

(provide (all-defined-out))

(struct signal  [] #:prefab)
(struct signal:weighted signal [s w] #:prefab)
(struct signal:sequence signal [n pat tempo function] #:prefab)
(struct signal:mix signal [ss] #:prefab)
(struct signal:drum signal [n pat tempo] #:prefab)
(struct signal:chord signal [notes beats] #:prefab)

(define (normalize signals)
  (define (normalize-pattern pat)
    (map (λ (p)
                (define notes (car p))
                (signal:chord (if (number? notes) (list notes) notes)
                              (cdr p)))
         pat))
  (match signals
    [`(,s) (normalize s)]
    [`(,s . ,n) #:when (number? n) (signal:weighted (normalize s) n)]
    [`(sequence ,n ,pat ,tempo ,wave)
     (signal:sequence n (normalize-pattern pat) tempo wave)]
    [`(drum ,n ,pat ,tempo) (signal:drum n pat tempo)]
    [`(mix . ,ss) (make-mix (map normalize ss))]
    [ss #:when (list? ss) (make-mix (map normalize ss))]))

(define (make-mix signals)
  (if (zero? (length (cdr signals)))
      (car signals)
      (signal:mix (map (λ (s) (if (signal:weighted? s) s (signal:weighted s 1))) signals))))
