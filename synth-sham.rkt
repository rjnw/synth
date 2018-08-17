#lang racket

(require "../sham/main.rkt")

(define sampling-frequency (make-parameter 44100))
(define bits-per-sample (make-parameter 16))

(define (freq->sample-period freq)
  (round (/ (sampling-frequency)
            freq)))
(define (seconds->samples s)
  (inexact->exact (round (* s (sampling-frequency)))))

(define wave-type (s$:tptr (s$:tfun (list s$:f32 s$:i32) s$:f32)))

(define sine-wave-function
  (s$:dfunction
   (void) 'sine-wave
   (list 'x 'freq) (list s$:i32 s$:f32) s$:f32
   (s$:ret (s$:app^ (s$:ri 'llvm.sin.f32 s$:f32)
                    (s$:app^ s$:fmul
                             (s$:app^ s$:fdiv (s$:app^ s$:fmul (s$:v 'freq) (s$:fl32 (* 2.0 pi)))
                                      (s$:fl32 (exact->inexact (sampling-frequency))))
                             (s$:app^ s$:ui->fp (s$:v 'x) (s$:etype s$:f32)))))))

(define (load-signal s i)
  (s$:app^  s$:load (s$:gep^ s i)))
(define (add-signal! s i v)
  (s$:app^ s$:store! (s$:app^ s$:fadd
                              (load-signal s i) v)
           (s$:gep^ s i)))

(define synthesize-note
  (s$:dfunction
   (void) 'synthesize-note
   '(freq nsamples wavef output output-startpos)
   (list s$:f32 s$:i32 wave-type s$:f32* s$:i32) s$:void
   (s$:se
    (s$:slet1^
     'ni s$:i32 (s$:ui32 0)
     (s$:while-ule^ (s$:v 'ni) (s$:v 'nsamples)
                    (s$:se
                     (add-signal! (s$:v 'output)
                                  (s$:app^ s$:add-nuw (s$:v 'ni) (s$:v 'output-startpos))
                                  (s$:app^ (s$:rs 'wavef)  (s$:v 'ni) (s$:v 'freq))))
                    (s$:set! (s$:v 'ni) (s$:app^ s$:add-nuw (s$:v 'ni) (s$:ui32 1))))
     s$:ret-void))))

(module+ test

  (require ffi/unsafe
           rackunit)
  (define test-module
    (s$:dmodule
     (empty-mod-env-info) 'test-module
     (list
      sine-wave-function
      synthesize-note
      )))

  (define mod-env (compile-module test-module))
  (jit-verify-module mod-env)
  (jit-dump-module mod-env)
  (optimize-module mod-env #:opt-level 3 #:size-level 2)
  (jit-dump-module mod-env)

  (initialize-jit! mod-env)

  (define s-note (jit-get-function 'synthesize-note mod-env))
  (define sine-wave (jit-get-function 'sine-wave mod-env))
  (define sine-wavef-ptr (jit-get-function-ptr 'sine-wave mod-env))

  (define tempo 3800)
  (define samples-per-beat (quotient (* (sampling-frequency) 60) tempo))
  (define mblock (malloc _float (* samples-per-beat 2)))
  (memset mblock 0 (* samples-per-beat 2) _float)
  (s-note 4.0 10 sine-wavef-ptr mblock 4)
  (check-= (ptr-ref mblock _float 6) (sine-wave 2 4.0) 0.00000001)
  )
