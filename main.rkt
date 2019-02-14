#lang racket

(require "synth-sham.rkt" "wave-params.rkt" "note.rkt" "synth-interp-vector.rkt")
(provide  (all-from-out "note.rkt")
         (rename-out [mix/export            mix]
                     [sequence/export       sequence]
                     [drum/export           drum]
                     [#%module-begin/export #%module-begin])
         (except-out (all-from-out racket) #%module-begin))

(require (for-syntax syntax/parse) racket/stxparam)

(define-syntax-parameter current-bpm (syntax-rules ()))

(begin-for-syntax
 (define-syntax-class mixand
   #:attributes (signal weight)
   (pattern [signal:expr (~datum #:weight) weight:expr])
   (pattern signal:expr #:with weight #'1)))

(define-syntax (mix/export stx)
  (syntax-parse stx
    [(_ sig:mixand ...)
     #'`(mix (,(cons sig.signal sig.weight) ...))]))

(begin-for-syntax
 (define-syntax-class sequend
   #:attributes (res)
   (pattern (~datum #f) ; break of 1 unit
            #:with res #'(cons #f 1))
   (pattern [(~datum #f) len:expr]
            #:with res #'(cons #f len))
   (pattern ((~literal chord) root octave duration type notes* ...)
            #:with res #'(chord 'root octave duration 'type notes* ...))
   (pattern ((~literal scale) root octave duration type notes* ...)
            #:with res #'(scale 'root octave duration 'type notes* ...))
   (pattern [n:id octave:expr] ; note of 1 unit
            #:with res #'(note 'n octave 1))
   (pattern [n:id octave:expr len:expr]
            #:with res #'(note 'n octave len))))

(define-syntax (sequence/export stx)
  (syntax-parse stx
    ;; TODO OoO keywords, support for repetitions, optional #:times
    [(_ function:id (~datum #:times) times:expr
        [note:sequend ...])
     #'`(sequence times ,(list note.res ...) ,(current-bpm) function)]))

(define-syntax (drum/export stx)
  (syntax-parse stx
    ;; TODO OoO keywords, support for repetitions, optional #:times
    [(_ (~datum #:times) times:expr [notes ...])
     #'`(drum times (notes ...) ,(current-bpm))]))

(module reader syntax/module-reader
  #:language 'synth)

(define-syntax (#%module-begin/export stx)
  (syntax-parse stx
    [(_ #:output output:expr #:bpm bpm:expr
        (~optional (~seq #:runner runner:expr) #:defaults ([runner #''interp]))
        signal ...)
     #'(#%module-begin
        (syntax-parameterize
            ([current-bpm (syntax-rules () [(_) bpm])])
          (if (equal? runner 'interp)
              (emit-interp (list signal ...) output)
              (emit (list signal ...) output))
          (void)))]))

(module+ test
  (syntax-parameterize
      ([current-bpm (syntax-rules () [(_) 0])])
    (mix/export (sequence/export sine-wave #:times 1
                                 [(chord C 3 3 major-arpeggio)
                                  (chord D 3 3 major-arpeggio)])
                [(sequence/export square-wave #:times 1
                                  [(C 4) (D 4) (E 4)
                                         (G 4) (E 4) (C 4)])
                 #:weight 3])))
