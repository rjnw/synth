#lang racket

(require
 ;; "../sham/main.rkt"
 "../sham/private/ast-utils.rkt"
 "sequencer.rkt"
 "signals.rkt"
 "wave-params.rkt")
(require ffi/unsafe)

(define bits-per-sample (make-parameter 16))

(define (freq->sample-period freq)
  (round (/ (sampling-frequency)
            freq)))
(define (seconds->samples s)
  (inexact->exact (round (* s (sampling-frequency)))))

(define (load-signal s i)
  (load (gep^ s i)))
(define (add-signal! s i v)
  (store! (fadd (load-signal s i) v)
             (gep^ s i)))


(define-function get-signal
  [(cblock : f32*) (index : i32)] f32
  (return (load-signal cblock index)))
(define-function set-signal
  [(cblock : f32*) (index : i32) (value : f32)] tvoid
  (store! value (gep^ cblock index))
  return-void)

(define wave-type (tptr (tfun (list i32 f32) f32)))
;; sham function: (frequency f32) (nsamples i32) (wavef (f32 i32 -> f32)) (output f32*) (offset i32)
;; starting at i 0->nsamples, output[i+offset] = wavef(i)
(define-function synthesize-note
  [(freq : f32) (nsamples : i32) (total-weight : f32) (wavef : wave-type) (output : f32*) (offset : i32)] tvoid
  (let^ ([ni (ui32 0) : i32])
        (while-ule^ ni nsamples
                    (add-signal! output
                                 (add-nuw ni offset)
                                 (fmul (app^ (rs 'wavef) ni freq)
                                       total-weight))
                    (set! ni (add-nuw ni (ui32 1))))
        return-void))


(define-function signal->integer
  [(x : f32) (gain : f32)] i32
  (let^ ([nx (fp->ui
              (fmul gain
                    (fmul (fadd x (fl32 1.0))
                          (fl32 (exact->inexact (expt 2 (sub1 ( bits-per-sample)))))))
              (etype i32))
             : i32])
    (if (icmp-ule (ui32 (expt 2 (sub1 ( bits-per-sample)))) nx)
        (return (ui32 (expt 2 (sub1 ( bits-per-sample)))))
        (return nx))))
