#lang racket

(require
 "wave-params.rkt"
 sham
 sham/ast-utils
 sham/jit-utils
 "drum.rkt"
 ffi/unsafe)

(provide lookup-wave
         sine-wave
         sawtooth-wave
         triangle-wave
         square-wave
         bass-drum-array
         snare-array)

(define (sample-period freq)
  (ri^ round.f32 f32 (fdiv (fl32 (exact->inexact (sampling-frequency))) freq)))
(define (sample-period/2 freq)
  (ri^ trunc.f32 f32 (fdiv (sample-period freq) (fl32 2.0))))
(define (sample-period/4 freq)
  (ri^ trunc.f32 f32 (fdiv (sample-period freq) (fl32 4.0))))
(define (x* x freq)
  (frem (ui->fp x (etype f32)) (sample-period freq)))

(define-sham-function
  (sine-wave [x : i32] [freq : f32] : f32)
  (ret (ri^ sin.f32 f32
            (fmul (fdiv (fmul freq (fl32 (* 2.0 pi)))
                        (fl32 (exact->inexact (sampling-frequency))))
                  (ui->fp  x (etype f32))))))

(define-sham-function
  #:info (function-info-add-attributes (empty-function-info) 'alwaysinline)
  (sawtooth-wave [x : i32] [freq : f32] : f32)
  (return (fsub (fdiv (x* x freq) (sample-period/2 freq))
             (fl32 1.0))))

(define-sham-function
  (triangle-wave [x : i32] [freq : f32] : f32)
   (if^ (fcmp-ugt (x* x freq) (sample-period/2 freq))
        (ret (fsub (fdiv (x* x freq) (sample-period/4 freq)) (fl32 3.0)))
        (ret (fadd (fdiv (x* x freq) (sample-period/4 freq)) (fl32 3.0)))))

(define-sham-function
  (square-wave [x : i32] [freq : f32] : f32)
               (if^ (fcmp-ugt (x* x freq) (sample-period/2 freq))
                    (ret (fl32 -1.0))
                    (ret (fl32 1.0))))

(define (lookup-wave w)
  (match w
    ['sawtooth-wave sawtooth-wave]
    ['sine-wave sine-wave]
    ['triangle-wave triangle-wave]
    ['square-wave square-wave]))


(define bass-drum-array (ptrcast (rptr->llvmptr (list->cblock bass-drum _float)) (etype i8*) (etype f32*)))
(define snare-array (ptrcast (rptr->llvmptr (list->cblock snare _float)) (etype i8*) (etype f32*)))

;; (module+ test
;;   (require ffi/unsafe
;;            rackunit)
;;   (define mod-env (compile-module
;;                    (dmodule (empty-mod-env-info) 'wave-module
;;                                (list
;;                                 sine-wave-function
;;                                 sawtooth-wave-function
;;                                 triangle-wave-function
;;                                 square-wave-function
;;                                 ))))
;;   (optimize-module mod-env #:opt-level 3)
;;   (jit-dump-module mod-env)
;;   (jit-verify-module mod-env)
;;   (initialize-jit! mod-env)
;;   (define sin (jit-get-function 'sine-wave mod-env))
;;   (define saw (jit-get-function 'sawtooth-wave mod-env))
;;   (define trn (jit-get-function 'triangle-wave mod-env))
;;   (define sqr (jit-get-function 'square-wave mod-env))


;;   (define (build-wave wave freq x)
;;     (define sample-period (freq->sample-period freq))
;;     (define sample-period/2 (quotient sample-period 2))
;;     (define sample-period/4 (quotient sample-period 4))
;;     (define x* (exact->inexact (modulo x sample-period)))
;;     (match wave
;;       ['sawtooth
;;        (- (/ x* sample-period/2) 1.0)]
;;       ['inverse-sawtooth
;;        (* -1.0 (- (/ x* sample-period/2) 1.0))]
;;       ['triangle
;;        (if (> x* sample-period/2)
;;            (- (/ x* sample-period/4) 3.0)
;;            (+ (/ x* sample-period/4 -1.0) 1.0))]
;;       ['square
;;        (if (> x* sample-period/2) -1.0 1.0)]
;;       ['sine
;;        (sin (* (exact->inexact (/ (* freq 2.0 pi) (sampling-frequency)))
;;                (exact->inexact x)))])))
