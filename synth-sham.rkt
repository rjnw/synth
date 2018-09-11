#lang racket

(require "../sham/main.rkt"
         "sequencer.rkt")
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
   (void) map-s->i
   '(wave gain nsamples)
   (list s$:f21 s$:f32 s$:i32)
   (s$:slet1^
    'ni s$:i32 (s$:ui32 0)
    (s$:while-ule^ (s$:v 'ni) (s$:v 'nsamples)
                   (s$:se (s$:app^ (s$:rs 'set-signal)
                                   (s$:v 'wave) (s$:v 'ni)
                                   (s$:app^ s$:fmul
                                            (s$:app^ (s$:rs 'signal->integer)
                                                     (s$:app^ (s$:rs 'get-signal) (s$:v 'wave) (s$:v 'ni)))
                                            (s$:v 'gain)))))
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
                                                  (s$:fl32 total-weight)
                                                  wv
                                                  (s$:v 'output)
                                                  ofst)))
                                apps)])))
       (list s$:ret-void)))))
  (values (list func) id))

(define (build-mix ss total-weight (id (gensym 'mix)))
  (match-define (cons signal weight) ss)
  (define downscale-ratio (/ 1.0 (apply + weight)))
  (define-values (lower-funcs app-ids)
    (for/fold ([lower-funcs '()]
               [app-ids '()])
              ([s ss])
      (define-values (fs id) (compile-signal (car s) (* (cdr s) downscale-ratio total-weight)))
      (values (append fs lower-funcs) (cons id app-ids))))
  (define func
    (s$:dfunction
     (void) (gensym 'mix)
     '(output offset)
     (list s$:f32* s$:i32) s$:void
     (s$:block
      (append
       (for/list ([app-id app-ids])
         (s$:app^ (s$:rs app-id) (s$:v 'output) (s$:v 'offset)))
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
    [`(mix ,sgnls) (apply max (map total-samples sgnls))]
    [`(sequence ,n ,pat ,tempo ,wave) (* (samples-in-pat pat tempo))]))

(define (signal-stream signal)
  (define nsamples (total-samples signal))
  (define memory-block (malloc _float nsamples))

  (error "add compile signal")
  (define (mblock-stream offset)
    (if (equal? offset nsamples)
        (begin
          (free memory-block)
          empty-stream)
        (stream-cons (ptr-ref memory-block _float offset)
                     (mblock-stream))))
  (mblock-stream 0))

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
  (define-values (seq-funs main-id)
    (compile-signal s 1.0))
  (define test-module
    (s$:dmodule
     (empty-mod-env-info) 'test-module
     (append
      (list
       sine-wave-function
       synthesize-note
       signal->integer
       get-signal
       set-signal)
      seq-funs)))

  (define mod-env (time (compile-module test-module)))

  (jit-dump-module mod-env)
  (jit-verify-module mod-env)

  (time (optimize-module mod-env #:opt-level 3 #:size-level 3))
  (jit-dump-module mod-env)

  (time (initialize-jit! mod-env))

  (define s-note (jit-get-function 'synthesize-note mod-env))
  (define sine-wave (jit-get-function 'sine-wave mod-env))
  (define sine-wavef-ptr (jit-get-function-ptr 'sine-wave mod-env))
  (define s->i (jit-get-function 'signal->integer mod-env))
  (define gs (jit-get-function 'get-signal mod-env))
  (define ss! (jit-get-function 'set-signal mod-env))

  (define mainf (jit-get-function main-id mod-env))

  (define nsamples (total-samples s))
  (define mblock (malloc _float nsamples))

  (begin (memset mblock 0 nsamples _float)
         (s-note 4.0 3 1.0 sine-wavef-ptr mblock 0)
         (pretty-display (cblock->list mblock _float 100)))
  (check-= (ptr-ref mblock _float 2) (sine-wave 2 4.0) 0.00000001)



  ;; (pretty-print
  ;;  (build-sequence
  ;;   1
  ;;   '((60 . 1) (#f . 1) (60 . 1) (#f . 1) (58 . 1) (#f . 1))
  ;;   380
  ;;   (s$:v 'sine-wave)))
  )
