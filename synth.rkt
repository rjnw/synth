#lang racket

(require math/array
         racket/stream)

(require "wav-encode.rkt"
         "sequencer.rkt")

(define fs 44100)                       ;sampling frequence 44100 samples per second
(define bits-per-sample 16)             ;16 bites * 44100 bits per second

(define (freq->sample-period freq)      ;number of samples for one period
  (round (/ fs freq)))

(define (seconds->samples s)
  (inexact->exact (round (* s fs))))

(struct wsignal [s w] #:prefab)                   ; signal and weight
(struct sequence [n pat tempo function] #:prefab)
(struct mix [ss] #:prefab)
(struct drum [pat] #:prefab)
(struct note [f beats] #:prefab)

;; returns a stream of float
(define (interp-mix sgls)
  (printf "interp-mix: sgnls: ~a\n" sgls)
  (define weights (map wsignal-w sgls))
  (define psgnls (map wsignal-s sgls))
  (define pstreams^ (map interp-signal psgnls))
  (define downscale-ratio (/ 1.0 (apply + weights)))
  (define (build-mix-stream pstreams)
    (stream-cons
     (foldl + 0
            (map (λ (s w) (* (if (stream-empty? s) 0 (stream-first s)) w downscale-ratio))
              pstreams weights))
     (if (andmap stream-empty? pstreams)
         empty-stream
         (build-mix-stream (map (λ (s o) (if (stream-empty? s) o (stream-rest s))) pstreams pstreams^)))))
  (build-mix-stream pstreams^))

(define (append-streams s1 s2)
  (if (stream-empty? s1)
      s2
      (stream-cons (stream-first s1) (append-streams (stream-rest s1) s2))))


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
     (sin (* (exact->inexact (/ (* freq 2.0 pi) fs))
             (exact->inexact x)))]))
(define (interp-sequence n pattern tempo wave)
  (printf "interp-sequence: n: ~a, pattern: ~a, tempo: ~a, wave: ~a\n" n pattern tempo wave)
  (define samples-per-beat (quotient (* fs 60) tempo))
  (define (synthesize-note nt)
    (match-define (note type beats) nt)
    (define nsamples (* beats samples-per-beat))
    (define (note-stream n)
      (if (equal? nsamples n)
          empty-stream
          (stream-cons
           (match type
             [#f 0]
             [x #:when (number? x) (build-wave wave (note-freq type) n)]
             [l #:when (list? l)  (foldl + 0 (map (λ (x) (/ (build-wave wave (note-freq x) n) 2)) l))])
           ;; (if type (build-wave wave (note-freq type) n) 0)
           (note-stream (add1 n)))))
    (note-stream 0))
  (define (synthesize-sequence pat)
    (if (empty? pat)
        empty-stream
        (let* ([cpat (first pat)])
          (append-streams (if (note? cpat)
                              (synthesize-note cpat)
                              (interp-mix cpat))
                          (synthesize-sequence (cdr pat))))))
  (synthesize-sequence pattern))

(define (interp-signal sgnl)
  (printf "interpreting: ~a\n" sgnl)
  (match sgnl
    [(mix sgnls) (interp-mix sgnls)]
    [(sequence n pat tempo wave) (interp-sequence n pat tempo wave)]
    [(drum pat) 0]))

(define (make-mix signals)
  (if (zero? (length (cdr signals)))
      (car signals)
      (mix (map (λ (s) (if (wsignal? s) s (wsignal s 1))) signals))))

(define (normalize signals)
  (define (normalize-pattern pat)
    (match pat
      [p (map (λ (p) (note (car p) (cdr p))) p)]))
  (match signals
    [`(,s) (normalize s)]
    [`(,s . ,n) #:when (number? n) (wsignal (normalize s) n)]
    [`(sequence ,n ,pat ,tempo ,wave)
     (sequence n (normalize-pattern pat) tempo wave)]
    [`(drum . ,d) (drum d)]
    [`(mix . ,ss) (make-mix (map normalize ss))]
    [ss #:when (list? ss) (make-mix (map normalize ss))]))

(define (total-samples signals)
  (define (samples-in-pat pat tempo)
    (define samples-per-beat (quotient (* fs 60) tempo))
    (foldr (λ (p t) (+ t (* samples-per-beat (note-beats p)))) 0 pat))
  (match signals
    [(mix sgnls) (apply max (map total-samples sgnls))]
    [(sequence n pat tempo wave) (* (samples-in-pat pat tempo))]
    [(wsignal s w) (total-samples s)]))
;; returns vector of floats
(define (create-signal-sequence signals)
  (printf "create-signal-sequence: normalizes-signals: ")
  (define ns (normalize signals))
  (pretty-display ns)
  (printf "total-samples: ~a\n" (total-samples ns))
  ;; (error 'stop)
  ;; (interp-signal ns)

  (define s (interp-signal ns))
  (signal->integer-sequence s #:gain 0.3)
  ;; (stream 0)
  )

(define (n-stream s n)
  (for/fold ([ns s]
             [out '()]
             #:result (reverse out))
            ([i (in-range n)])
    (values  (stream-rest ns)
             (cons (stream-first ns) out))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; assumes array of floats in [-1.0,1.0]
;; assumes gain in [0,1], which determines how loud the output is
(define (signal->integer-sequence signal #:gain [gain 1])
  (stream-map
   (λ (sample)
     (max 0 (min (sub1 (expt 2 bits-per-sample)) ; clamp
                 (exact-floor
                  (* gain
                     (* (+ sample 1.0) ; center at 1, instead of 0
                        (expt 2 (sub1 bits-per-sample))))))))
   signal)
  ;; (for/vector #:length (vector-length signal)
  ;;     ([sample (in-vector signal)])
  ;;   (max 0 (min (sub1 (expt 2 bits-per-sample)) ; clamp
  ;;               (exact-floor
  ;;                (* gain
  ;;                   (* (+ sample 1.0) ; center at 1, instead of 0
  ;;                      (expt 2 (sub1 bits-per-sample))))))))
  )

(define (emit signals file)
  ;; (pretty-display signals)
  (printf "emitting: ~a\n" signals)
  (define signal-sequence (create-signal-sequence signals))
  (with-output-to-file file #:exists 'replace
    (lambda () (write-wav signal-sequence))))

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
