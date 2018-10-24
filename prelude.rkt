#lang racket

(require
 ;; "../sham/main.rkt"
 "../sham/private/ast-utils.rkt"
 "../sham/private/parameters.rkt"
 "note.rkt"
 "signals.rkt"
 "wave-params.rkt")
(require ffi/unsafe)

(provide (all-defined-out))

(define (load-signal s i)
  (load (gep^ s i)))
(define (add-signal! s i v)
  (store! (fadd (load-signal s i) v)
             (gep^ s i)))

(define-sham-function (get-signal (cblock : f32*) (index : i32)) : f32
  (return (load-signal cblock index)))
(define-sham-function (set-signal (cblock : f32*) (index : i32) (value : f32)) : tvoid
  (store! value (gep^ cblock index))
  return-void)

(define wave-type (tptr (tfun (list i32 f32) f32)))
;; sham function: (frequency f32) (nsamples i32) (wavef (f32 i32 -> f32)) (output f32*) (offset i32)
;; starting at i 0->nsamples, output[i+offset] = wavef(i)
(define-sham-function
  (synthesize-note (freq : f32) (nsamples : i32) (total-weight : f32) (wavef : wave-type) (output : f32*) (offset : i32)) : tvoid
  (slet^ ([ni (ui32 0) : i32])
         (while-ule^ ni nsamples
                     (add-signal! output
                                  (add-nuw ni offset)
                                  (fmul (app^ wavef ni freq)
                                        total-weight))
                     (set!^ ni (add-nuw ni (ui32 1))))
         return-void))

(define-sham-function
  (signal->integer (x : f32) (gain : f32)) : i32
  (slet^ ([nx (fp->ui
               (fmul gain
                     (fmul (fadd x (fl32 1.0))
                           (fl32 (exact->inexact (expt 2 (sub1 (bits-per-sample)))))))
               (etype i32))
              : i32])
         (if^ (icmp-ule (ui32 (expt 2 (sub1 (bits-per-sample)))) nx)
              (return (ui32 (expt 2 (sub1 (bits-per-sample)))))
              (return nx))))

(define-sham-function
  (map-s->i (wave : f32*) (gain : f32) (nsamples : i32)) : tvoid
  (slet^ ([ni (ui32 0) : i32])
         (while-ule^ ni nsamples
                     (let^ ([nv (signal->integer (get-signal wave ni) gain)
                                : i32]
                            [casted-wave (ptrcast wave (etype i32*)) : i32*])
                           (store! nv (gep^ casted-wave ni)))
                     (set!^ ni (add-nuw ni (ui32 1))))
         return-void))

;; (module+ test
;;   (require "../sham/main.rkt")
;;   (define-module test-module
;;     (empty-mod-env-info)
;;     (list get-signal
;;           set-signal
;;           synthesize-note
;;           signal->integer
;;           map-s->i
;;           ))
;;   (define test-mod-env (jit-module-compile test-module))
;;   (jit-verify-module test-mod-env)
;;   (optimize-module test-mod-env #:opt-level 3)
;;   (initialize-jit! test-mod-env)
;;   (jit-dump-module test-mod-env)

;;   (define gs (jit-get-function 'get-signal test-mod-env))
;;   (define ss! (jit-get-function 'set-signal test-mod-env))


;;   (define s-note (jit-get-function 'synthesize-note test-mod-env))
;;   (define s->i (jit-get-function 'signal->integer test-mod-env))
;;   (define ms->i (jit-get-function 'map-s->i test-mod-env))



;;   (define mblock (malloc _double 10000))
;;   (memset mblock 0 10000 _double)

;;   )
