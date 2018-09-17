#lang racket
(require "../sham/main.rkt")

(provide wave-type
         sine-wave-function
         sampling-frequency
         ;; sawtooth-wave-function
         )

(define sampling-frequency (make-parameter 44100))

(define wave-type (s$:tptr (s$:tfun (list s$:i32 s$:f32) s$:f32)))

(define sine-wave-function
  (s$:dfunction
   (void) 'sine-wave
   (list 'x 'freq) (list s$:i32 s$:f32) s$:f32
   (s$:ret (s$:ri-sin f32 s$:f32
                      (s$:fmul (s$:fdiv (s$:fmul (s$:v 'freq) (s$:fl32 (* 2.0 pi)))
                                        (s$:fl32 (exact->inexact (sampling-frequency))))
                               (s$:ui->fp (s$:v 'x) (s$:etype s$:f32)))))))

(define (sample-period freq)
  (round (s$:fdiv (sampling-frequency) freq)))

;; (define sawtooth-wave-function
;;   (s$:dfunction
;;    (void) 'sawtooth-wave
;;    (list 'x 'freq) (list s$:i32 s$:f32) s$:f32
;;    (s$:ret (s$:fsub (s$:fdiv (modulo 'x sample-period)
;;                              (s$:fdiv sample-period))
;;                     1.0))))
