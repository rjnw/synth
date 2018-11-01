#lang racket

(require "../sham/main.rkt"
         "../sham/private/parameters.rkt"
         "../sham/private/ast-utils.rkt"
         "../sham/private/jit-utils.rkt"
         "signals.rkt"
         "wave-builder.rkt"
         "wave-params.rkt"
         "wave-sham.rkt"
         "prelude.rkt"
         "wav-encode.rkt")

(provide emit)
(require ffi/unsafe)

(define (compile-signal signal (total-weight 1.0))
  (define f
    (match signal
      [(signal:sequence n pat tempo wave)
       (build-sequence n pat tempo (lookup-wave wave) total-weight)]
      [(signal:mix ss)
       (define weights (map signal:weighted-w ss))
       (define downscale-ratio (/ 1.0 (apply + weights)))
       (define ssfs
         (map (λ (ws)
                (match-define (signal:weighted s w) ws)
                (compile-signal s (* w downscale-ratio total-weight)))
              ss))
       (build-mix ssfs)]
      [(signal:drum n pat tempo)
       (build-drum n pat tempo total-weight)]))
  (add-to-sham-module! (current-sham-module) f)
  f)

(define (total-samples signals)
  (define (samples-per-beat tempo)
    (quotient (* (sampling-frequency) 60) tempo))
  (define (samples-in-pat pat tempo)
    (foldr (λ (p t) (+ t (* (samples-per-beat tempo) (signal:chord-beats p)))) 0 pat))
  (match signals
    [(signal:weighted s w) (total-samples s)]
    [(signal:sequence n pat tempo wave) (* n (samples-in-pat pat tempo))]
    [(signal:mix ss) (apply max (map total-samples ss))]
    [(signal:drum n pat tempo) (* n (length pat) (samples-per-beat tempo))]))

(define (emit signal-sym file-name)
  (define signal (normalize signal-sym))
  (define nsamples (total-samples signal))
  (define memory-block (malloc _double nsamples 'raw))
  (memset memory-block 0 nsamples _float)

  (define (mblock-stream offset)
    (if (equal? offset nsamples)
        empty-stream
        (stream-cons (ptr-ref memory-block _uint offset)
                     (mblock-stream (add1 offset)))))

  (define mainf
    (time (compile-signal signal)))

  (time
   (parameterize
       ([compile-options `(;; dump verify pretty
                           mc-jit
                           ,@(compile-options))])
     (compile-sham-module!
      (current-sham-module)
      #:opt-level 3 #:size-level 3
      #:loop-vec #f)))

  (time (begin (sham-app mainf memory-block 0)
               (sham-app map-s->i memory-block 0.3 nsamples)))

  (define signal-stream (mblock-stream 0))
  (time (with-output-to-file file-name #:exists 'replace
     (λ () (write-wav memory-block nsamples))))
  (free memory-block)
  )

(module+ test
  (require ffi/unsafe
           rackunit)

  (define ft
    '((sequence
       10
       (
        (60 . 5)
        (#f . 1) (60 . 1) (#f . 1) (58 . 1) (#f . 1)
        (60 . 1) (#f . 3) (55 . 1) (#f . 3) (55 . 1) (#f . 1)
        (60 . 1) (#f . 1) (65 . 1) (#f . 1) (64 . 1) (#f . 1)
        (60 . 1) (#f . 9)
        )
       380
       sawtooth-wave)
      ;; (drum 8 (O #f #f #f X #f #f #f) 380)
      ))
  (emit ft "funky-town-sham.wav")


  (define m
    '((mix
       (((sequence 1 (((36 40 43) . 3) ((38 42 45) . 3)) 60 sine) . 1)
        ((sequence
          1
          ((48 . 1) (50 . 1) (52 . 1) (55 . 1) (52 . 1) (48 . 1))
          60
          square)
         .
         3)))))
  ;; (emit m "melody-sham.wav")
  )
