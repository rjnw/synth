#lang racket

(require "../sham/main.rkt"
         "sequencer.rkt"
         "wav-encode.rkt")
(require ffi/unsafe)
(define sampling-frequency (make-parameter 44100))
(define bits-per-sample (make-parameter 16))

(define (freq->sample-period freq)
  (round (/ (sampling-frequency)
            freq)))
(define (seconds->samples s)
  (inexact->exact (round (* s (sampling-frequency)))))

(define wave-type (s$:tptr (s$:tfun (list s$:i32 s$:f32) s$:f32)))

(define sine-wave-function
  (s$:dfunction
   (void) 'sine-wave
   (list 'x 'freq) (list s$:i32 s$:f32) s$:f32
   (s$:ret (s$:app^ (s$:ri 'llvm.sin.f32 s$:f32)
                    (s$:fmul (s$:fdiv (s$:fmul (s$:v 'freq) (s$:fl32 (* 2.0 pi)))
                                      (s$:fl32 (exact->inexact (sampling-frequency))))
                             (s$:ui->fp (s$:v 'x) (s$:etype s$:f32)))))))

(define (load-signal s i)
  (s$:load (s$:gep^ s i)))
(define (add-signal! s i v)
  (s$:store! (s$:fadd (load-signal s i) v)
             (s$:gep^ s i)))

(define get-signal
  (s$:dfunction
   (void) 'get-signal
   '(cblock index)
   (list s$:f32* s$:i32) s$:f32
   (s$:ret (load-signal (s$:v 'cblock) (s$:v 'index)))))
(define set-signal
  (s$:dfunction
   (void) 'set-signal
   '(cblock index value)
   (list s$:f32* s$:i32 s$:f32) s$:void
   (s$:block^ (s$:se (s$:store! (s$:v 'value)
                               (s$:gep^ (s$:v 'cblock) (s$:v 'index))))
              s$:ret-void)))
;; sham function: (frequency f32) (nsamples i32) (wavef (f32 i32 -> f32)) (output f32*) (offset i32)
;; starting at i 0->nsamples, output[i+offset] = wavef(i)
(define synthesize-note
  (s$:dfunction
   (void) 'synthesize-note
   '(freq nsamples total-weight wavef output offset)
   (list s$:f32 s$:i32 s$:f32 wave-type s$:f32* s$:i32) s$:void
   (s$:slet1^
    'ni s$:i32 (s$:ui32 0)
    (s$:while-ule^ (s$:v 'ni) (s$:v 'nsamples)
                   (s$:se
                    (add-signal! (s$:v 'output)
                                 (s$:add-nuw (s$:v 'ni) (s$:v 'offset))
                                 (s$:fmul (s$:app^ (s$:rs 'wavef)  (s$:v 'ni) (s$:v 'freq))
                                          (s$:v 'total-weight))))
                   (s$:set! (s$:v 'ni) (s$:add-nuw (s$:v 'ni) (s$:ui32 1))))
    s$:ret-void)))

(define signal->integer
  (s$:dfunction
   (void) 'signal->integer
   '(x gain) (list s$:f32 s$:f32) s$:i32
   (s$:slet1^
    'nx s$:i32 (s$:fp->ui (s$:fmul (s$:v 'gain)
                                   (s$:fmul (s$:fadd (s$:v 'x) (s$:fl32 1.0))
                                            (s$:fl32 (exact->inexact (expt 2 (sub1 ( bits-per-sample)))))))
                          (s$:etype s$:i32))
    (s$:if (s$:icmp-ule (s$:ui32 (expt 2 (sub1 ( bits-per-sample)))) (s$:v 'nx))
           (s$:ret (s$:ui32 (expt 2 (sub1 ( bits-per-sample)))))
           (s$:ret (s$:v 'nx))))))
(define map-s->i
  (s$:dfunction
   (void) 'map-s->i
   '(wave gain nsamples)
   (list s$:f32* s$:f32 s$:i32) s$:void
   (s$:slet1^
    'ni s$:i32 (s$:ui32 0)
    (s$:while-ule^ (s$:v 'ni) (s$:v 'nsamples)
                   (s$:slet1^ 'nv s$:i32 (s$:app^ (s$:rs 'signal->integer)
                                                  (s$:app^ (s$:rs 'get-signal) (s$:v 'wave) (s$:v 'ni))
                                                  (s$:v 'gain))
                              (s$:slet1^ 'casted-wave s$:i32* (s$:ptrcast (s$:v 'wave) (s$:etype s$:i32*))
                                         (s$:se (s$:store! (s$:v 'nv) (s$:gep^ (s$:v 'casted-wave) (s$:v 'ni))))))
                   (s$:set! (s$:v 'ni) (s$:add-nuw (s$:v 'ni) (s$:ui32 1))))
    s$:ret-void)))

(define (build-sequence n pattern tempo wave total-weight (id (gensym 'sequence)))
  (pretty-print pattern)
  (define wv (s$:v wave))
  (define samples-per-beat (quotient (* (sampling-frequency) 60) tempo))
  (define func
    (s$:dfunction
     (void) id
     '(output offset)
     (list s$:f32* s$:i32) s$:void
     (s$:block
      (append
       (for/fold ([ofst (s$:v 'offset)]
                  [apps '()]
                  #:result (reverse apps))
                 ([p pattern])
         (match-define (cons note beats) p)
         (define nsamples (* samples-per-beat beats))
         (values (s$:add-nuw ofst (s$:ui32 nsamples))
                 (cond [(false? note) apps]
                       [(number? note)
                        (cons (s$:se
                               (s$:app^ (s$:rs 'synthesize-note)
                                        (s$:fl32 (note-freq note))
                                        (s$:ui32 nsamples)
                                        (s$:fl32 total-weight)
                                        wv
                                        (s$:v 'output)
                                        ofst))
                              apps)]
                       [(list? note)
                        (append (for/list ([n note])
                                  (s$:se (s$:app^ (s$:rs 'synthesize-note)
                                                  (s$:fl32 (note-freq n))
                                                  (s$:ui32 nsamples)
                                                  (s$:fdiv (s$:fl32 total-weight)
                                                           (s$:fl32 (exact->inexact (length note))))
                                                  wv
                                                  (s$:v 'output)
                                                  ofst)))
                                apps)])))
       (list s$:ret-void)))))
  (pretty-print func)
  (values (list func) id))

(define (build-mix ss total-weight (id (gensym 'mix)))
  (printf "build-mix: ss ~a\n" ss)
  (define weights (map cdr ss))
  ;; (define signals (map car ss))
  (define downscale-ratio (/ 1.0 (apply + weights)))
  (define-values (lower-funcs app-ids)
    (for/fold ([lower-funcs '()]
               [app-ids '()])
              ([s ss])
      (define-values (fs id) (compile-signal (car s) (* (cdr s) downscale-ratio total-weight)))
      (values (append fs lower-funcs) (cons id app-ids))))
  (define func
    (s$:dfunction
     (void) id
     '(output offset)
     (list s$:f32* s$:i32) s$:void
     (s$:block
      (append
       (for/list ([app-id app-ids])
         (s$:se (s$:app^ (s$:rs app-id) (s$:v 'output) (s$:v 'offset))))
       (list s$:ret-void)))))
  (values (cons func lower-funcs) id))

(define (compile-signal signal total-weight)
  (match signal
    [`(sequence ,n ,pat ,tempo ,wave)
     (build-sequence n pat tempo wave total-weight)]
    [`(mix . ,ss) (build-mix ss total-weight)]))

(define (total-samples signals)
  (define (samples-in-pat pat tempo)
     (define samples-per-beat (quotient (* (sampling-frequency) 60) tempo))
    (foldr (λ (p t) (+ t (* samples-per-beat (cdr p)))) 0 pat))
  (match signals
    [`(mix  (,sgnls . ,w) ...) (apply max (map total-samples sgnls))]
    [`(sequence ,n ,pat ,tempo ,wave) (* (samples-in-pat pat tempo))]))

(define (emit-signal signal file-name)
  (define nsamples (total-samples signal))
  (define memory-block (malloc _float nsamples))
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
       sine-wave-function
       synthesize-note
       signal->integer
       get-signal
       set-signal
       map-s->i)
      seq-funs)))
  (define mod-env (compile-module signal-module))
  ;; (optimize-module mod-env #:opt-level 3 #:size-level 3)
  (initialize-jit! mod-env)
  (define mainf (jit-get-function main-id mod-env))
  (define ms->i (jit-get-function 'map-s->i mod-env))
  (mainf memory-block 0)
  (ms->i memory-block 0.3 nsamples)
  (define signal-stream (mblock-stream 0))

  (with-output-to-file file-name #:exists 'replace
    (λ () (write-wav signal-stream)))
  (values mainf mod-env memory-block))

(module+ test

  (require ffi/unsafe
           rackunit)

  (define s
    `(sequence 1
               (
                (60 . 1) (#f . 1) (60 . 1) (#f . 1) (58 . 1) (#f . 1)
                (60 . 1) (#f . 3) (55 . 1) (#f . 3) (55 . 1) (#f . 1)
                (60 . 1) (#f . 1) (65 . 1) (#f . 1) (64 . 1) (#f . 1)
                (60 . 1) (#f . 9))
               380
               sine-wave))
  ;; (define-values (mainf mod-env mblock) (emit-signal s "funky-town-sham.wav"))
  (define chord-test
    ;; `(mix ( (sequence 1
    ;;                    ;; (((36 40 43) . 3))
    ;;                    (
    ;;                     (36 . 3))
    ;;                    60 sine-wave) . 1)
    ;;        ( (sequence 1
    ;;                    ;; (((36 40 43) . 3))
    ;;                    (
    ;;                     (40 . 3))
    ;;                    60 sine-wave)  . 1)
    ;;        ( (sequence 1
    ;;                   ;; (((36 40 43) . 3))
    ;;                   (
    ;;                    (43 . 3))
    ;;                   60 sine-wave) . 1))
    `(sequence 1
              (((36 40 43) . 3))
              ;; ( (36 . 3) (40 . 3) (43 . 3))
              400 sine-wave)
    )
  (define sm
    `(mix
      ((sequence 1
                 ;; ((60 . 1) (#f . 1) (60 . 1) (#f . 1) (58 . 1) (#f . 1))
                 (((36 40 43) . 3) ((38 42 45) . 3))
                 60 sine-wave) . 1)
      ((sequence
        1
        ((48 . 1) (50 . 1) (52 . 1) (55 . 1) (52 . 1) (48 . 1))
        60
        sine-wave)
       .
       3)))
  (define-values (mainf mod-env mblock) (emit-signal sm "melody-sham.wav"))

  ;; (jit-dump-module mod-env)
  ;; (error 'stop)

  (define s-note (jit-get-function 'synthesize-note mod-env))
  (define sine-wave (jit-get-function 'sine-wave mod-env))
  (define sine-wavef-ptr (jit-get-function-ptr 'sine-wave mod-env))
  (define s->i (jit-get-function 'signal->integer mod-env))
  (define ms->i (jit-get-function 'map-s->i mod-env))
  (define gs (jit-get-function 'get-signal mod-env))
  (define ss! (jit-get-function 'set-signal mod-env))

  (define nsamples (total-samples s))

  (begin (memset mblock 0 nsamples _float)
         (s-note 4.0 3 1.0 sine-wavef-ptr mblock 0))
  (check-= (ptr-ref mblock _float 2) (sine-wave 2 4.0) 0.00000001)

  (memset mblock 0 nsamples _float)
  (mainf mblock 0)
  (pretty-display (cblock->list mblock _float 10))

  (ms->i mblock 0.3 nsamples)
  (pretty-display (cblock->list mblock _uint 10)))
