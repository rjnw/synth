#lang racket
(require
 ;; "../sham/private/ast-utils.rkt"
 "../sham/ast.rkt")

(define sampling-frequency (make-parameter 44100))

(define wave-type (s$:tptr (s$:tfun (list s$:i32 s$:f32) s$:f32)))

(define sine-wave-function
  (s$:dfunction
   (void) 'sine-wave
   (list 'x 'freq) (list s$:i32 s$:f32) s$:f32
   (s$:ret (s$:app^ (s$:ri-rint f32 s$:f32
                               (s$:fmul (s$:fdiv (s$:fmul (s$:v 'freq) (s$:fl32 (* 2.0 pi)))
                                                 (s$:fl32 (exact->inexact (sampling-frequency))))
                                        (s$:ui->fp (s$:v 'x) (s$:etype s$:f32))))))))
