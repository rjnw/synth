#lang racket

(require math/array
         racket/stream)

(require "wav-encode.rkt"
         "wave-params.rkt"
         "signals.rkt")

;; returns a stream of float
(define (build-wave wave freq x)
  (define sample-period (freq->sample-period freq))
  (define sample-period/2 (quotient sample-period 2))
  (define sample-period/4 (quotient sample-period 4))
  (define x* (exact->inexact (modulo x sample-period)))
  (match wave
    ['sawtooth
     (- (/ x* sample-period/2) 1.0)]
    ['inverse-sawtooth
     (* -1.0 (- (/ x* sample-period/2) 1.0))]
    ['triangle
     (if (> x* sample-period/2)
         (- (/ x* sample-period/4) 3.0)
         (+ (/ x* sample-period/4 -1.0) 1.0))]
    ['square
     (if (> x* sample-period/2) -1.0 1.0)]
    ['sine
     (sin (* (exact->inexact (/ (* freq 2.0 pi) (sampling-frequency)))
             (exact->inexact x)))]))

(define (interp-sequence n pattern tempo wave total-weight)
  (printf "interp-sequence: n: ~a, pattern: ~a, tempo: ~a, wave: ~a\n" n pattern tempo wave)
  (λ (output offset)
    (for ([i (in-range n)])
      (for/fold ([ofst offset])
                ([p pattern])
        (match-define (signal:chord notes beats) p)
        (define nsamples (* samples-per-beat beat))
        (cond  [(false? notes) (void)]
               [(list? notes)
                (for ([n notes])
                  (interp-note (note-freq n) nsamples (/ total-weight (exact->inexact (length notes))) wave output ofst))])
        (+ ofst nsamples)))))
(define (interp-mix ssfs)
  (λ (output offset)
    (for ([f ssfs])
      (f output offset))))

(define (interp-drum n pattern tempo total-weight)
  (define samples-per-beat (quotient (* (sampling-frequency) 60) tempo))
  (define beat-samples (seconds->samples 0.05))
  (define (copy-vector to ofst from)
    (for ([i (in-range beat-samples)])
      (add-signal! to (+ i ofst) (* (get-signal from) total-weight))))
  (λ (output offset)
    (for ([i (in-range n)])
      (for/fold ([ofst offset])
                ([p pattern])
        (match p
          ['X (copy-vector output ofst bass-drum)]
          ['O (copy-vector output ofst snare)]
          [#f (svoid)])
        (+ ofst samples-per-beat)))))

(define (interp-signal sgnl (total-weight 1))
  (printf "interpreting: ~a\n" sgnl)
  (match sgnl
    [(signal:sequence n pat tempo wave) (interp-sequence n pat tempo (lookup-wave wave) total-weight)]
    [(signal:mix sgnls)
       (define weights (map signal:weighted-w ss))
       (define downscale-ratio (/ 1.0 (apply + weights)))
       (define ssfs
         (map (λ (ws)
                (match-define (signal:weighted s w) ws)
                (interp-signal s (* w downscale-ratio total-weight)))
              ss))
       (build-mix ssfs)]
    [(signal:drum n pat tempo) (interp-drum n pat tempo total-weight)]))


(define (total-samples signals)
  (define (samples-in-pat pat tempo)
    (define samples-per-beat (quotient (* (sampling-frequency) 60) tempo))
    (foldr (λ (p t) (+ t (* samples-per-beat (signal:chord-beats p)))) 0 pat))
  (match signals
    [(signal:mix sgnls) (apply max (map total-samples sgnls))]
    [(signal:sequence n pat tempo wave) (* (samples-in-pat pat tempo))]
    [(signal:weighted s w) (total-samples s)]))

(define (emit signal-sym file-name)
  (define signal (normalize signal-sym))
  (define nsamples (total-samples signal))
  (define block (build-vector (const 0) nsamples))
  (define entry-signal (interp-signal signal))
  (time (entry-signal block))
  (time (signal->integer-sequence block 0.3))
  (time (with-output-to-file file-name #:exists 'replace
          (λ () (write-wav block nsamples)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; assumes array of floats in [-1.0,1.0]
;; assumes gain in [0,1], which determines how loud the output is
(define (signal->integer-sequence signal #:gain [gain 1])
  ;; change to update vector inline with mutation
  (for/vector #:length (vector-length signal)
      ([sample (in-vector signal)])
    (max 0 (min (sub1 (expt 2 bits-per-sample)) ; clamp
                (exact-floor
                 (* gain
                    (* (+ sample 1.0) ; center at 1, instead of 0
                       (expt 2 (sub1 bits-per-sample)))))))))

(module+ test
  (define s
    (create-signal-sequence
     '((sequence
        1
        (
         (60 . 1) (#f . 1) (60 . 1) (#f . 1) (58 . 1) (#f . 1)
         (60 . 1) (#f . 3) (55 . 1) (#f . 3) (55 . 1) (#f . 1)
         (60 . 1) (#f . 1) (65 . 1) (#f . 1) (64 . 1) (#f . 1)
         (60 . 1) (#f . 9))
        380
        sine))))
  ;; (stream->list s)
  ;; (n-stream s 100)
  ;; smoke-on-the-water
  ;; (create-signal-sequence
  ;;  '((sequence
  ;;     1
  ;;     (
  ;;      ((38 43) . 1) (#f . 1) ((41 46) . 1) (#f . 1) ((43 48) . 2) (#f . 1)
  ;;      ((38 43) . 1) (#f . 1) ((41 46) . 1) (#f . 1) ((44 49) . 1) ((43 48) . 2) (#f . 2)
  ;;      ((38 43) . 1) (#f . 1) ((41 46) . 1) (#f . 1) ((43 48) . 2) (#f . 1) ((41 46) . 1) (#f . 1)
  ;;      ((38 43) . 5)
  ;;      )
  ;;     224
  ;;     sawtooth-wave)
  ;;    (drum 2 (O #f X #f O #f X #f O #f X #f O O X X) 224)))

  ;; funky-town
  (emit
   '((sequence
      1
      (
       (60 . 1) (#f . 1) (60 . 1) (#f . 1) (58 . 1) (#f . 1)
       (60 . 1) (#f . 3) (55 . 1) (#f . 3) (55 . 1) (#f . 1)
       (60 . 1) (#f . 1) (65 . 1) (#f . 1) (64 . 1) (#f . 1)
       (60 . 1) (#f . 9)
       )
      380
      sine))
   "funky-town-stream.wav")

  ;; melody
  (emit
   '((mix
      (((sequence 1 (((36 40 43) . 3) ((38 42 45) . 3)) 60 sine) . 1)
       ((sequence
         1
         ((48 . 1) (50 . 1) (52 . 1) (55 . 1) (52 . 1) (48 . 1))
         60
         square)
        .
        3))))
   "melody.wav")

  ;; (emit '(sequence 1 (((36 40 43) . 3) ((38 42 45) . 3)) 60 square) "melody.wav")
  #;(emit
     '((sequence
        1
        (((38 43) . 1) (#f . 1) ((41 46) . 1) (#f . 1) ((43 48) . 2) (#f . 1)
                       ((38 43) . 1) (#f . 1) ((41 46) . 1) (#f . 1) ((44 49) . 1) ((43 48) . 2) (#f . 2)
                       ((38 43) . 1) (#f . 1) ((41 46) . 1) (#f . 1) ((43 48) . 2) (#f . 1) ((41 46) . 1) (#f . 1)
                       ((38 43) . 5))
        224
        sawtooth-wave)
       (drum 2 (O #f X #f O #f X #f O #f X #f O O X X) 224))
     "smoke-on-the-water.wav"))
