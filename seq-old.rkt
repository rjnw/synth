
;; Accepts notes or pauses, but not chords.
(define (synthesize-note note n-samples function)
  ;; (define note-block (malloc _double n-samples 'atomic))

  ;; (printf "synthesize-note note: ~a, n-samples: ~a\n" note n-samples)
  (define synth (if note (function (note-freq note)) #f))
  ;; (for ([i (in-range n-samples)])
  ;;   (ptr-set! note-block i (if note ((third synth) i) 0.0)))
  ;; (print note-block)
  ;; (print "synth:") (pretty-display synth) (newline)

  (define ret
    (build-array (vector n-samples)
                 (if note
                     (compose (third synth) array-lambda-compose)
                     (lambda (x) 0.0))))
  (define retv (build-vector n-samples (if note (third synth) (lambda (x) 0.0))))
  (for ([a (in-array ret)]
        [v retv]
        [i (in-range n-samples)])
    (unless (equal? a v)
      (printf "not equal ~a\n" i)))
  ;; (pretty-display ret)
  retv) ; pause

;; repeats n times the sequence encoded by the pattern, at tempo bpm
;; pattern is a list of either single notes (note . duration) or
;; chords ((note ...) . duration) or pauses (#f . duration)
(define (sequence n pattern tempo function)
  (printf "sequence: pattern: ~a\n" pattern)
  (define samples-per-beat (quotient (* fs 60) tempo))
  (printf "sequencefs: ~a, samples-per-beat: ~a\n" fs samples-per-beat)
  (apply vector-append
         (for*/list ([i    (in-range n)] ; repeat the whole pattern
                     [note (in-list  pattern)])
           ;; (printf "sequence:for* i: ~a, note: ~a\n" i note)
           (if (list? (car note)) ; chord
               (apply mix
                      (for/list ([x (in-list (car note))])
                        (list (synthesize-note x
                                               (* samples-per-beat (cdr note))
                                               function)
                              1))) ; all of equal weight
               (synthesize-note (car note)
                                (* samples-per-beat (cdr note))
                                function)))))

;; (cons (vector float) real)* -> (vector float)
(define (mix-vector . ss)
  (printf "mix-vector: n: ~a, args: ~a" (length ss) ss)
  (define signals (map car ss))
  (define weights (map cdr ss))
  (define lengths (map vector-length signals))
  (define downscale-ratio (/ 1.0 (apply + weights)))
  ;; (define ((scale-signal s) x) (* x w downscale-signal))

  (define total-samples (apply max lengths))

  (define retv
    (build-vector
     total-samples
     (λ (i)
       (apply +
              (map (λ (ss)
                     (define s (car ss))
                     (define w (cdr ss))
                     (* w downscale-ratio
                        (vector-ref s (modulo i (vector-length s)))))
                   ss)))))
  retv)
