#lang racket

(require "../sham/main.rkt"
         "sequencer.rkt")

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

;; sham function: (frequency f32) (nsamples i32) (wavef (f32 i32 -> f32)) (output f32*) (offset i32)
;; starting at i 0->nsamples, output[i+offset] = wavef(i)
(define synthesize-note
  (s$:dfunction
   (void) 'synthesize-note
   '(freq nsamples wavef output offset)
   (list s$:f32 s$:i32 wave-type s$:f32* s$:i32) s$:void
   (s$:slet1^
    'ni s$:i32 (s$:ui32 0)
    (s$:while-ule^ (s$:v 'ni) (s$:v 'nsamples)
                   (s$:se
                    (add-signal! (s$:v 'output)
                                 (s$:add-nuw (s$:v 'ni) (s$:v 'offset))
                                 (s$:app^ (s$:rs 'wavef)  (s$:v 'ni) (s$:v 'freq))))
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

(define (build-sequence n pattern tempo wave)
  (pretty-print pattern)
  (define samples-per-beat (quotient (* (sampling-frequency) 60) tempo))
  (s$:dfunction
   (void) (gensym 'sequence)
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
                                      wave
                                      (s$:v 'output)
                                      ofst))
                            apps)]
                     [(list? note) (error "chords not done yet")])))
     (list s$:ret-void)))))


(module+ test

  (require ffi/unsafe
           rackunit)
  (define test-module
    (s$:dmodule
     (empty-mod-env-info) 'test-module
     (list
      sine-wave-function
      synthesize-note
      signal->integer
      (build-sequence
       1
       '(
         (60 . 1) (#f . 1) (60 . 1) (#f . 1) (58 . 1) (#f . 1)
         (60 . 1) (#f . 3) (55 . 1) (#f . 3) (55 . 1) (#f . 1)
         (60 . 1) (#f . 1) (65 . 1) (#f . 1) (64 . 1) (#f . 1)
         (60 . 1) (#f . 9))
       380
        (s$:v 'sine-wave))
      )))

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

  (define tempo 3800)
  (define samples-per-beat (quotient (* (sampling-frequency) 60) tempo))
  (define mblock (malloc _float (* samples-per-beat 2)))
  (memset mblock 0 (* samples-per-beat 2) _float)
  (s-note 4.0 10 sine-wavef-ptr mblock 4)
  (check-= (ptr-ref mblock _float 6) (sine-wave 2 4.0) 0.00000001)



  ;; (pretty-print
  ;;  (build-sequence
  ;;   1
  ;;   '((60 . 1) (#f . 1) (60 . 1) (#f . 1) (58 . 1) (#f . 1))
  ;;   380
  ;;   (s$:v 'sine-wave)))
  )
