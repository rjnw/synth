#lang racket

(require "../sham/main.rkt"
         "../sham/private/ast-utils.rkt"
         "signals.rkt"
         "wave-builder.rkt"
         "wave-params.rkt"
         "wave-sham.rkt"
         "prelude.rkt"
         "wav-encode.rkt")

(provide emit)
(require ffi/unsafe)

(define (compile-signal signal total-weight)
  (match signal
    [(signal:sequence n pat tempo wave)
     (build-sequence n pat tempo wave total-weight)]
    [(signal:mix ss)
     (define weights (map signal:weighted-w ss))
     ;; (define signals (map car ss))
     (define downscale-ratio (/ 1.0 (apply + weights)))
     (define-values (lower-funcs ssfs)
       (for/fold ([lower-funcs '()]
                  [ssfs '()])
                 ([ws ss])
         (match-define (signal:weighted s w) ws)
         (define-values (f lfs) (compile-signal s (* w downscale-ratio total-weight)))
         (values (append lfs lower-funcs) (cons f ssfs))))

     (build-mix ssfs total-weight)]
    [(signal:drum n pat tempo)
     (build-drum n pat tempo total-weight)]))

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
  (define-values (seq-funs main-id)
    (compile-signal signal 1.0))
  (define signal-module
    (s$:dmodule
     (empty-mod-env-info) 'signal-module
     (append
      (list
       sine-wave
       square-wave
       triangle-wave
       sawtooth-wave
       synthesize-note
       signal->integer
       get-signal
       set-signal
       map-s->i)
      seq-funs)))
  (define mod-env (jit-module-compile signal-module))
  (jit-verify-module mod-env)
  (optimize-module mod-env #:opt-level 3 #:size-level 3 #:loop-vec #t)
  ;; (jit-verify-module mod-env)
  ;; (jit-dump-module mod-env)
  (initialize-jit! mod-env)
  (define mainf (jit-get-function main-id mod-env))
  (define ms->i (jit-get-function 'map-s->i mod-env))
  (mainf memory-block 0)
  (ms->i memory-block 0.3 nsamples)
  (define signal-stream (mblock-stream 0))
  (with-output-to-file file-name #:exists 'replace
    (λ () (write-wav signal-stream)))
  (printf "done writing, freeing memory.\n")
  (free memory-block)
  (values mainf mod-env))

(module+ test

  (require ffi/unsafe
           rackunit)

  (define s
    '(
      (sequence
       1
       (
        (60 . 1) (#f . 1) (60 . 1) (#f . 1) (58 . 1) (#f . 1)
        (60 . 1) (#f . 3) (55 . 1) (#f . 3) (55 . 1) (#f . 1)
        (60 . 1) (#f . 1) (65 . 1) (#f . 1) (64 . 1) (#f . 1)
        (60 . 1) (#f . 9))
       380
       sawtooth-wave)
      (drum 8 (O #f #f #f X #f #f #f) 380)
      )
    ;; `(sequence 1
    ;;            (
    ;;             (60 . 1) (#f . 1) (60 . 1) (#f . 1) (58 . 1) (#f . 1)
    ;;             (60 . 1) (#f . 3) (55 . 1) (#f . 3) (55 . 1) (#f . 1)
    ;;             (60 . 1) (#f . 1) (65 . 1) (#f . 1) (64 . 1) (#f . 1)
    ;;             (60 . 1) (#f . 9))
    ;;            380
    ;;            sine-wave)
    )
  (define-values (mainf mod-env) (time (emit s "funky-town-sham.wav")))

  (define sm
    '((mix
       (((sequence 1 (((36 40 43) . 3) ((38 42 45) . 3)) 60 sine) . 1)
        ((sequence
          1
          ((48 . 1) (50 . 1) (52 . 1) (55 . 1) (52 . 1) (48 . 1))
          60
          square)
         .
         3)))))
  ;; (define-values (mainf mod-env mblock) (time (emit sm "melody-sham.wav")))

  ;; (define s-note (jit-get-function 'synthesize-note mod-env))
  ;; (define sine-wave (jit-get-function 'sine-wave mod-env))
  ;; (define sine-wavef-ptr (jit-get-function-ptr 'sine-wave mod-env))
  ;; (define s->i (jit-get-function 'signal->integer mod-env))
  ;; (define ms->i (jit-get-function 'map-s->i mod-env))
  ;; (define gs (jit-get-function 'get-signal mod-env))
  ;; (define ss! (jit-get-function 'set-signal mod-env))

  ;; (define nsamples (total-samples (normalize s)))

  ;; (define mblock (malloc _float nsamples))

  ;; (begin (memset mblock 0 nsamples _float)
  ;;        (s-note 4.0 3 1.0 sine-wavef-ptr mblock 0))
  ;; (check-= (ptr-ref mblock _float 2) (sine-wave 2 4.0) 0.00000001)

  ;; (memset mblock 0 nsamples _float)
  ;; (mainf mblock 0)
  ;; (pretty-display (cblock->list mblock _float 10))

  ;; (ms->i mblock 0.3 nsamples)
  ;; (pretty-display (cblock->list mblock _uint 10))
  )
