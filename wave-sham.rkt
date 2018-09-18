#lang racket

(require
 "wave-params.rkt"
 "../sham/main.rkt")

(provide wave-type
         sine-wave-function
         sampling-frequency
         ;; sawtooth-wave-function
         )



(define wave-type (s$:tptr (s$:tfun (list s$:i32 s$:f32) s$:f32)))

(define sine-wave-function
  (s$:dfunction
   (void) 'sine-wave
   (list 'x 'freq) (list s$:i32 s$:f32) s$:f32
   (s$:ret (s$:ri^ sin.f32 s$:f32
                      (s$:fmul (s$:fdiv (s$:fmul (s$:v 'freq) (s$:fl32 (* 2.0 pi)))
                                        (s$:fl32 (exact->inexact (sampling-frequency))))
                               (s$:ui->fp (s$:v 'x) (s$:etype s$:f32)))))))

(define (sample-period freq)
  (s$:ri^ round.f32 s$:f32 (s$:fdiv (s$:fl32 (exact->inexact (sampling-frequency))) freq)))

(define (sample-period/2 freq)
  (s$:ri^ trunc.f32 s$:f32 (s$:fdiv (s$:fl32 (exact->inexact (sampling-frequency))) (s$:fl32 2.0))))
(define (sample-period/4 freq)
  (s$:ri^ trunc.f32 s$:f32 (s$:fdiv (s$:fl32 (exact->inexact (sampling-frequency))) (s$:fl32 4.0))))
(define (x* x freq)
  (s$:frem (s$:ui->fp x (s$:etype s$:f32)) (sample-period freq)))
(define sawtooth-wave-function
  (s$:dfunction
   (void) 'sawtooth-wave
   (list 'x 'freq) (list s$:i32 s$:f32) s$:f32
   (s$:ret (s$:fsub (s$:fdiv (x* (s$:v 'x) (s$:v 'freq))
                             (sample-period/2 (s$:v 'freq)))
                    (s$:fl32 1.0)))))
(define triangle-wave-function
  (s$:dfunction
   (void) 'triangle-wave
   (list 'x 'freq) (list s$:i32 s$:f32) s$:f32
   (s$:if (s$:fcmp-ugt (x* (s$:v 'x) (s$:v 'freq)) (sample-period/2 (s$:v 'freq)))
          (s$:ret (s$:fsub
            (s$:fdiv (x* (s$:v 'x) (s$:v 'freq))
                     (sample-period/4 (s$:v 'freq)))
            (s$:fl32 3.0)))
          (s$:ret (s$:fadd
            (s$:fdiv (x* (s$:v 'x) (s$:v 'freq))
                     (sample-period/4 (s$:v 'freq)))
            (s$:fl32 3.0))))))


(define square-wave-function
  (s$:dfunction
   (void) 'square-wave
   (list 'x 'freq) (list s$:i32 s$:f32) s$:f32
   (s$:if (s$:fcmp-ugt (x* (s$:v 'x) (s$:v 'freq)) (sample-period/2 (s$:v 'freq)))
          (s$:ret (s$:fl32 -1.0))
          (s$:ret (s$:fl32 1.0)))))

(module+ test
  (require ffi/unsafe
           rackunit)
  (define mod-env (compile-module
                   (s$:dmodule (empty-mod-env-info) 'wave-module
                               (list
                                sine-wave-function
                                sawtooth-wave-function
                                triangle-wave-function
                                square-wave-function
                                ))))
  (optimize-module mod-env #:opt-level 3)
  (jit-dump-module mod-env)
  (jit-verify-module mod-env)
  (initialize-jit! mod-env)
  (define sin (jit-get-function 'sine-wave mod-env))
  (define saw (jit-get-function 'sawtooth-wave mod-env))
  (define trn (jit-get-function 'triangle-wave mod-env))
  (define sqr (jit-get-function 'square-wave mod-env)))
